# --- Local File Locations ---
$localCobol = "COBOL\CALCDVOP.cbl"
$localComp = "JCL\COMPJCL.jcl"
$localRun = "JCL\RUNJCL.jcl"


# --- 1. SECURITY & CONFIG ---
$myUSER_ID = $env:ZOWE_USER
$myPASSWORD = $env:ZOWE_PASSWORD

if (-not $myUSER_ID -or -not $myPASSWORD) {
   Write-Error "CRITICAL: ZOWE_USER or ZOWE_PASSWORD not found in Environment Variables!"
   exit
}

$CBL_PDS = "$myUSER_ID.ZMYPRSNL.COBOL"
$JCL_PDS = "$myUSER_ID.ZMYPRSNL.JCL"
$LOG_DIR = ".\logs"

# Ensure log directory exists
if (!(Test-Path $LOG_DIR)) { New-Item -ItemType Directory -Path $LOG_DIR }

# Start recording everything to a variable for the log file
$sessionOutput = New-Object System.Collections.Generic.List[string]
function Write-Log($msg, $color = "White") {
   Write-Host $msg -ForegroundColor $color
   $sessionOutput.Add("$(Get-Date -Format 'HH:mm:ss') - $msg")
}

Write-Log ">>> VIBEGARDEN DEVOPS PIPELINE STARTING <<<" "Cyan"

# --- 2. LINTING (Local check) ---
Write-Log "[1/6] Running Static Analysis (Linting)..." "Yellow"
$content = Get-Content $localCobol
if ($content -match "PROCEDURE DIVISION" -and $content -match "PROGRAM-ID") {
   Write-Log "PASS: COBOL Structure valid." "Green"
}
else {
   Write-Log "FAIL: COBOL Structure invalid!" "Red"; exit
}

# --- 3. GIT SYNC ---
Write-Log "[2/6] Syncing Source to GitHub..." "Yellow"
git add .
git commit -m "Auto-Deploy: $(Get-Date)"
git push origin main

# --- 4. MAINFRAME UPLOAD ---
Write-Log "[3/6] Uploading files to $USER_ID libraries..." "Yellow"
zowe files upload file-to-data-set $localCobol  "$CBL_PDS(CALCDVOP)" --user $myUSER_ID --pass $myPASSWORD
zowe files upload file-to-data-set $localComp   "$JCL_PDS(COMPJCL)"  --user $myUSER_ID --pass $myPASSWORD
zowe files upload file-to-data-set $localRun    "$JCL_PDS(RUNJCL)"   --user $myUSER_ID --pass $myPASSWORD


# --- 5. COMPILE ---
Write-Log "[4/6] Submitting Compile Job..." "Yellow"
$rawOutput = zowe jobs submit data-set "$JCL_PDS(COMPJCL)" --wait-for-output --view-all-spool-content --user $myUSER_ID --pass $myPASSWORD --rfj
$compJob = $rawOutput | ConvertFrom-Json

if ($null -eq $compJob.data) {
   Write-Log "CRITICAL ERROR: Zowe did not return valid JSON data." "Red"
   Write-Host "Raw output was: $rawOutput" -ForegroundColor Gray
   exit 1
}

Write-Log $compJob.data.retcode  "Red"

$rc = $compJob.data.retcode
Write-Log "Mainframe Return Code: $rc" "Cyan"

if ($compJob.data.retcode -eq " CC 0000") {
   Write-Log "SUCCESS: Compilation Clean." "Green"

   # --- 6. AUTO-TEST ---
   Write-Log "[5/6] Running Automated Unit Test..." "Yellow"
   $runJob = zowe jobs submit data-set "$JCL_PDS(RUNJCL)" --wait-for-output --view-all-spool-content --user $myUSER_ID --pass $myPASSWORD

   $expected = "VibeGarden Result: 150"
   if ($runJob -match $expected) {
      Write-Log "TEST PASSED: Output matches expectation." "Green"
   }
   else {
      Write-Log "TEST FAILED: Logic error detected." "Red"; exit
   }
}
else {
   Write-Log "COMPILE FAILED. Check SYSOUT." "Red"; exit
}

# --- 7. FINAL AUDIT LOG ---
$finalLogFile = "$LOG_DIR\Deploy_$(Get-Date -Format 'yyyyMMdd_HHmm').log"
$sessionOutput | Out-File -FilePath $finalLogFile
Write-Log "[6/6] Pipeline Complete. Audit Log: $finalLogFile" "Cyan"