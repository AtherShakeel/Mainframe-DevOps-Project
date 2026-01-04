import subprocess
import os
import sys
import datetime
import json
import re
import time

# ==============================================================================
# 1. PRE-FLIGHT DEPENDENCY CHECK
# ==============================================================================
dependencies_passed = True

def check_cmd(cmd, name):
    global dependencies_passed
    check = subprocess.run(f"where {cmd}", shell=True, capture_output=True)
    if check.returncode != 0:
        print(f"\033[91m[ERROR] {name} is NOT installed.\033[0m")
        dependencies_passed = False

# A & B: Check Git and Zowe
check_cmd("git", "Git")
check_cmd("zowe", "Zowe CLI")

# C. Check Credentials (Matching your $env names)
z_user = os.environ.get("ZOWE_USER")
z_pass = os.environ.get("ZOWE_PASSWORD")

if not z_user or not z_pass:
    print("\033[91m[ERROR] Missing Credentials: ZOWE_USER and ZOWE_PASSWORD must be set.\033[0m")
    dependencies_passed = False

# D. Check Folders
for folder in ["COBOL", "JCL"]:
    if not os.path.exists(folder):
        print(f"\033[91m[ERROR] Missing Folder: {folder}\033[0m")
        dependencies_passed = False

if not dependencies_passed:
    sys.exit(1)

print("\033[96m All dependencies verified. Starting VibeGarden Master Pipeline...\033[0m")

# ==============================================================================
# 2. CONFIGURATION & LOGGING
# ==============================================================================
JCL_PDS = f"{z_user}.ZMYPRSNL.JCL"
COBOL_PDS = f"{z_user}.ZMYPRSNL.COBOL"

log_dir = "./logs"
if not os.path.exists(log_dir): os.makedirs(log_dir)
log_file = os.path.join(log_dir, f"deploy-{datetime.datetime.now().strftime('%Y%m%d-%H%M')}.md")

def write_log(msg, color_name):
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    # Terminal Colors (Matching Write-Host)
    colors = {"Green": "\033[92m", "Red": "\033[91m", "Cyan": "\033[96m", "Yellow": "\033[93m", "White": "\033[97m"}
    print(f"{colors.get(color_name, '')}[{timestamp}] {msg}\033[0m")

    # Markdown Logic (Matching your switch statement)
    md_map = {"Green": "## PASS:", "Red": "## ERROR:", "Cyan": "> SYNC:", "Yellow": "- INFO:"}
    md_line = f"{md_map.get(color_name, ' ')} {msg} [{timestamp}]  \n"
    with open(log_file, "a") as f: f.write(md_line)

# ==============================================================================
# 3. START STOPWATCH
# ==============================================================================
start_time = datetime.datetime.now()

# ==============================================================================
# 4. PRE-DEPLOYMENT GIT RECORD
# ==============================================================================
write_log("[1/7] Recording local changes (Local Commit)...", "Yellow")
subprocess.run("git add .", shell=True)
subprocess.run(f"git commit -m \"VibeGarden Build Started: {start_time.strftime('%H:%M')}\" --allow-empty", shell=True)

# ==============================================================================
# 5. VERSIONING LOGIC (BUILD-TAG Replacement)
# ==============================================================================
new_tag = datetime.datetime.now().strftime("%Y%m%d-B%H%M")
write_log(f"Updating COBOL Version Tag to: {new_tag}", "Cyan")

cobol_path = "COBOL/CALCDVOP.cbl"
with open(cobol_path, 'r') as f:
    content = f.read().replace("BUILD-TAG", new_tag)
with open(cobol_path, 'w') as f:
    f.write(content)

# ==============================================================================
# 6. MAINFRAME UPLOAD
# ==============================================================================
write_log(f"[2/7] Uploading Source to {z_user}...", "Yellow")
files_to_upload = [
    (f"COBOL/CALCDVOP.cbl", f"{COBOL_PDS}(CALCDVOP)"),
    (f"JCL/COMPJCL.jcl", f"{JCL_PDS}(COMPJCL)"),
    (f"JCL/RUNJCL.jcl", f"{JCL_PDS}(RUNJCL)")
]

for local, remote in files_to_upload:
    cmd = f"zowe files upload file-to-data-set \"{local}\" \"{remote}\" --user {z_user} --pass {z_pass}"
    subprocess.run(cmd, shell=True, capture_output=True)

# ==============================================================================
# 7. COMPILE SECTION
# ==============================================================================
write_log("[3/7] Compiling COBOL...", "Yellow")
comp_cmd = f"zowe jobs submit data-set \"{JCL_PDS}(COMPJCL)\" --wait-for-output --rfj --user {z_user} --pass {z_pass}"
comp_res = subprocess.run(comp_cmd, shell=True, capture_output=True, text=True)

try:
    comp_data = json.loads(comp_res.stdout)
    rc = comp_data.get("data", {}).get("retcode", "UNKNOWN")
except:
    rc = "UNKNOWN (Zowe Communication Error)"

if rc not in ["CC 0000", "CC 0004"]:
    write_log(f"[ERROR] COMPILE FAILED: {rc}. Check spool for errors.", "Red")
    sys.exit(1)
write_log(" COMPILE SUCCESS", "Green")

# ==============================================================================
# 8. EXECUTION SECTION
# ==============================================================================
write_log("[4/7] Running Automated TESTS (RUNJCL)...", "Yellow")
run_cmd = f"zowe jobs submit data-set \"{JCL_PDS}(RUNJCL)\" --wait-for-output --rfj --user {z_user} --pass {z_pass}"
run_res = subprocess.run(run_cmd, shell=True, capture_output=True, text=True)
run_data = json.loads(run_res.stdout)
job_id = run_data.get("data", {}).get("jobid")

# Dynamic Spool Lookup
spool_cmd = f"zowe jobs list spool-files-by-jobid {job_id} --rfj --user {z_user} --pass {z_pass}"
spool_list = json.loads(subprocess.run(spool_cmd, shell=True, capture_output=True, text=True).stdout)
sysout_id = next((f["id"] for f in spool_list.get("data", []) if f["ddname"].strip() == "SYSOUT"), None)

if not sysout_id: sysout_id = spool_list["data"][-1]["id"]

test_results = subprocess.run(f"zowe jobs view spool-file-by-id {job_id} {sysout_id} --user {z_user} --pass {z_pass}", shell=True, capture_output=True, text=True).stdout

# ==============================================================================
# 9. VALIDATION & GITHUB PUSH
# ==============================================================================
write_log("[5/7] Validating Result Logic...", "Yellow")
print("\033[90m--- PROGRAM OUTPUT ---\n" + test_results.strip() + "\n----------------------\033[0m")

regex_pattern = r"VibeGarden Result:\s*([\d,.]+)"
match = re.search(regex_pattern, test_results)

if match:
    found_value = match.group(1).strip()
    write_log(f"  TESTS PASSED: Captured Result: {found_value}", "Green")

    write_log("[6/7] Success! Pushing 'Blessed' code to GitHub...", "Yellow")
    subprocess.run(f"git commit --amend -m \"VibeGarden Success: Job {job_id} - Result {found_value}\"", shell=True)
    subprocess.run("git push", shell=True)
    write_log(" GITHUB SYNCED", "Cyan")
else:
    write_log("[ERROR] TEST FAILED: Could not extract numeric result.", "Red")
    sys.exit(1)

# ==============================================================================
# 10. DURATION CALCULATION
# ==============================================================================
duration = datetime.datetime.now() - start_time
minutes, seconds = divmod(duration.seconds, 60)
write_log(f"[7/7] VibeGarden Pipeline Finished Successfully in {minutes} min {seconds} sec!", "Yellow")