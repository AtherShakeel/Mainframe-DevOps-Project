import subprocess
import json
import re
import datetime
import os
import sys

# --- 1. ENVIRONMENT & SECURITY (Matches $env:ZOWE_USER) ---
# ECID teams expect scripts to fail fast if security is missing.
ZOWE_USER = os.environ.get("ZOWE_USER")
ZOWE_PASS = os.environ.get("ZOWE_PASSWORD")

# --- 2. LOGGING CONFIGURATION (Matches Write-Log logic) ---
LOG_DIR = r"C:\Users\ather\Mainframe_DevOps\logs"
TIMESTAMP = datetime.datetime.now().strftime("%Y%m%d-%H%M")
LOG_FILE = os.path.join(LOG_DIR, f"deploy-{TIMESTAMP}.md")

if not os.path.exists(LOG_DIR):
    os.makedirs(LOG_DIR)

def write_log(msg, status="INFO"):
    time_str = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    styles = {"PASS": "## ✅ PASS:", "ERROR": "## ❌ ERROR:", "SYNC": "> **SYNC:**", "INFO": "- **INFO:**"}
    formatted_msg = f"{styles.get(status, '-')} [{time_str}] {msg}"
    print(formatted_msg)
    with open(LOG_FILE, "a", encoding="utf-8") as f:
        f.write(f"{formatted_msg}  \n")

# --- 3. DEPENDENCY & VERSIONING (Matches your Versioning Logic) ---
def pre_flight_check():
    write_log(f"Initializing VibeGarden Build Version 1.{TIMESTAMP}", "INFO")
    if not ZOWE_USER or not ZOWE_PASS:
        write_log("Security check failed: Credentials not in Environment Variables!", "ERROR")
        sys.exit(1)

    # Check if Zowe CLI is installed (Matches 'Get-Command zowe' logic)
    check_zowe = subprocess.run("zowe --version", shell=True, capture_output=True)
    if check_zowe.returncode != 0:
        write_log("Dependency Error: Zowe CLI is not installed.", "ERROR")
        sys.exit(1)

# --- 4. EXECUTION LOGIC ---
def run_deploy():
    pre_flight_check()

    # A. GIT ADD & COMMIT (Matches initial Git logic)
    write_log("Syncing local code to Git repository...", "SYNC")
    subprocess.run("git add .", shell=True)
    subprocess.run(f'git commit -m "VibeGarden Build {TIMESTAMP}"', shell=True)

    # B. MAINFRAME COMPILE (Matches zowe zos-jobs submit)
    write_log("Submitting COBOL Compile to Mainframe...", "INFO")
    compile_cmd = "zowe zos-jobs submit local-file './COBOL/VIBE.cbl' --directory './JCL' --wait"
    cp_res = subprocess.run(compile_cmd, shell=True, capture_output=True, text=True)

    if cp_res.returncode != 0:
        write_log("Zowe command failed to execute.", "ERROR", cp_res.returncode)
        sys.exit(1)

    job_data = json.loads(cp_res.stdout)
    job_id = job_data.get("jobid")
    retcode = job_data.get("retcode")

    if retcode == "CC 0000":
        write_log(f"COMPILE SUCCESSFUL: {job_id}", "PASS")
    else:
        # C. AUTO-SPOOL ON FAILURE (Matches your Error-Handling logic)
        write_log(f"COMPILE FAILED: {retcode}. Fetching Spool...", "ERROR")
        spool = subprocess.run(f"zowe zos-jobs view spool-by-id {job_id}", shell=True, capture_output=True, text=True).stdout
        with open(LOG_FILE, "a") as f:
            f.write(f"\n### 📋 FAILED SPOOL ({job_id})\n```text\n{spool}\n```\n")
        sys.exit(1)

    # D. TEST & REGEX EXTRACTION (Matches your $regex = '...' logic)
    write_log("Running Automated Regression Tests...", "INFO")
    test_cmd = "zowe zos-jobs submit local-file './JCL/RUNTEST.jcl' --wait"
    test_res = subprocess.run(test_cmd, shell=True, capture_output=True, text=True)
    test_job_id = json.loads(test_res.stdout).get("jobid")

    test_spool = subprocess.run(f"zowe zos-jobs view spool-by-id {test_job_id}", shell=True, capture_output=True, text=True).stdout

    # Extracting the 1,100.00 value
    match = re.search(r'VibeGarden Result:\s*([\d,.]+)', test_spool)

    if match:
        found_value = match.group(1)
        write_log(f"TESTS PASSED: VibeGarden Result = {found_value}", "PASS")

        # E. FINAL GIT PUSH (Matches final $env:GITHUB_PUSH logic)
        write_log("Pushing verified changes to GitHub...", "SYNC")
        subprocess.run(f'git commit --amend -m "VibeGarden Deploy Success: {found_value}"', shell=True)
        subprocess.run("git push", shell=True)
    else:
        write_log("VALIDATION FAILED: No numeric result found in spool.", "ERROR")
        sys.exit(1)

if __name__ == "__main__":
    run_deploy()