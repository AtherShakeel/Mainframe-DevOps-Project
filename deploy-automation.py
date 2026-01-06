import subprocess
import os
import sys
import datetime
import json
import re
import argparse

# ==============================================================================
# 1. ARGUMENT & PATH CONFIGURATION (Jenkins Ready)
# ==============================================================================
# Add NPM path so Jenkins finds Zowe on Windows
os.environ["PATH"] += os.pathsep + os.path.join(os.environ.get('APPDATA', ''), 'npm')
# This stops Zowe from hanging while waiting for SSL confirmation
os.environ["ZOWE_OPT_REJECT_UNAUTHORIZED"] = "false"

# --- YOUR CRITICAL PATHS (DO NOT REMOVE) ---
os.environ["PATH"] += os.pathsep + r"C:\Users\ather\AppData\Local\Programs\Python\Python314"
os.environ["PATH"] += os.pathsep + r"C:\Users\ather\AppData\Local\Programs\Python\Python314\Scripts"

parser = argparse.ArgumentParser()
parser.add_argument("--user", required=True, help="Mainframe User ID")
parser.add_argument("--passw", required=True, help="Mainframe Password")
args = parser.parse_args()

z_user = args.user
z_pass = str(args.passw) # Force string to ensure masking works 100%

# ==============================================================================
# 2. PRE-FLIGHT CHECK
# ==============================================================================
def check_cmd(cmd, name):
    check = subprocess.run(f"where {cmd}", shell=True, capture_output=True)
    if check.returncode != 0:
        print(f"\033[91m[ERROR] {name} is NOT installed.\033[0m")
        sys.exit(1)

check_cmd("zowe", "Zowe CLI")

for folder in ["COBOL", "JCL"]:
    if not os.path.exists(folder):
        print(f"\033[91m[ERROR] Missing Folder: {folder}\033[0m")
        sys.exit(1)

print("\033[96m Dependencies verified. Starting Mainframe Automation  Master Pipeline...\033[0m")

# ==============================================================================
# 3. CONFIGURATION & LOGGING
# ==============================================================================
JCL_PDS = f"{z_user}.ZMYPRSNL.JCL"
COBOL_PDS = f"{z_user}.ZMYPRSNL.COBOL"

log_dir = "./logs"
if not os.path.exists(log_dir): os.makedirs(log_dir)
log_file = os.path.join(log_dir, f"deploy-{datetime.datetime.now().strftime('%Y%m%d-%H%M')}.md")

def write_log(msg, color_name):
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    # Switched to standard ANSI (31-36) for better visibility in Jenkins
    colors = {
        "Green": "\033[32m",   # Darker Green
        "Red": "\033[31m",     # Darker Red
        "Cyan": "\033[34m",    # Blue (easier to read than Cyan)
        "Yellow": "\033[33m"   # Gold/Brown (much better on white logs)
    }
    print(f"{colors.get(color_name, '')}[{timestamp}] {msg}\033[0m")

    md_map = {"Green": "## PASS:", "Red": "## ERROR:", "Cyan": "> SYNC:", "Yellow": "- INFO:"}
    md_line = f"{md_map.get(color_name, ' ')} {msg} [{timestamp}]  \n"
    with open(log_file, "a") as f: f.write(md_line)

start_time = datetime.datetime.now()

# ==============================================================================
# 4. VERSIONING LOGIC
# ==============================================================================
new_tag = datetime.datetime.now().strftime("%Y%m%d-B%H%M")
write_log(f"Updating COBOL Version Tag to: {new_tag}", "Cyan")

cobol_path = "COBOL/CALCDVOP.cbl"
if os.path.exists(cobol_path):
    with open(cobol_path, 'r') as f:
        content = f.read().replace("BUILD-TAG", new_tag)
    with open(cobol_path, 'w') as f:
        f.write(content)

# ==============================================================================
# 5. MAINFRAME UPLOAD
# ==============================================================================
write_log(f"Uploading Source to {z_user}...", "Yellow")
files_to_upload = [
    ("COBOL/CALCDVOP.cbl", f"{COBOL_PDS}(CALCDVOP)"),
    ("JCL/COMPJCL.jcl", f"{JCL_PDS}(COMPJCL)"),
    ("JCL/RUNJCL.jcl", f"{JCL_PDS}(RUNJCL)")
]

for local, remote in files_to_upload:
    cmd = f"zowe files upload file-to-data-set \"{local}\" \"{remote}\" --user {z_user} --pass {z_pass} --ru false"
    # MASKING: Print command to console without the password
    log_cmd = cmd.replace(z_pass, "********")
    # This removes 'capture_output' so Jenkins can see everything
    print(f"Executing: {' '.join(log_cmd)}") # This helps us see the exact command being run
    subprocess.run(cmd, shell=True)

# ==============================================================================
# 6. COMPILE & EXECUTE
# ==============================================================================
write_log("Compiling COBOL...", "Yellow")
comp_cmd = f"zowe jobs submit data-set \"{JCL_PDS}(COMPJCL)\" --wait-for-output --rfj --user {z_user} --pass {z_pass} --ru false"

# Mask the compile command too
print(f"Executing: {comp_cmd.replace(z_pass, '********')}")
# We MUST capture output here so the json.loads() below has data to read
comp_res = subprocess.run(comp_cmd, shell=True, capture_output=True, text=True)
print(comp_res.stdout) # Manually print it so you can see it in Jenkins

try:
    rc = json.loads(comp_res.stdout).get("data", {}).get("retcode", "UNKNOWN")
except:
    rc = "ERROR"

if rc not in ["CC 0000", "CC 0004"]:
    write_log(f"COMPILE FAILED: {rc}", "Red")
    sys.exit(1)

write_log("Running Tests...", "Yellow")
run_cmd = f"zowe jobs submit data-set \"{JCL_PDS}(RUNJCL)\" --wait-for-output --rfj --user {z_user} --pass {z_pass} --ru false"

print(f"Executing: {run_cmd.replace(z_pass, '********')}")
run_res = subprocess.run(run_cmd, shell=True, capture_output=True, text=True)
print(run_res.stdout)

try:
    run_data = json.loads(run_res.stdout)
    job_id = run_data.get("data", {}).get("jobid")
except:
    write_log("Failed to parse Job ID from Run result", "Red")
    sys.exit(1)

# Spool Retrieval
# A. List all spool files for the job
spool_cmd = f"zowe jobs list spool-files-by-jobid {job_id} --rfj --user {z_user} --pass {z_pass} --ru false"

# B. Mask it before printing so Jenkins is safe
print(f"Executing: {spool_cmd.replace(z_pass, '********')}")

spool_res = subprocess.run(spool_cmd, shell=True, capture_output=True, text=True)
spool_list = json.loads(spool_res.stdout)

# C. Identify the correct SYSOUT ID
all_sysouts = [f["id"] for f in spool_list.get("data", []) if f["ddname"].strip() == "SYSOUT"]
sysout_id = all_sysouts[-1] if all_sysouts else spool_list["data"][-1]["id"]

# D. View the specific spool file content
view_cmd = f"zowe jobs view spool-file-by-id {job_id} {sysout_id} --user {z_user} --pass {z_pass} --ru false"
# SECURITY FIX: Mask the view command before printing
print(f"Executing: {view_cmd.replace(z_pass, '********')}")

# Run and capture the actual test results
test_results_res = subprocess.run(view_cmd, shell=True, capture_output=True, text=True)
test_results = test_results_res.stdout

# PRINT the result so it appears in Jenkins logs
print(test_results)

# ==============================================================================
# 7. VALIDATION
# ==============================================================================
write_log("Validating Output...", "Yellow")
match = re.search(r"VibeGarden Result:\s*([\d,.]+)", test_results)

if match:
    write_log(f"TESTS PASSED: Result {match.group(1)}", "Green")
else:
    write_log("TEST FAILED: No numeric result found.", "Red")
    sys.exit(1)

duration = datetime.datetime.now() - start_time
write_log(f"VibeGarden Finished in {duration.seconds // 60}m {duration.seconds % 60}s", "Yellow")