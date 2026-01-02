# --- Local File Locations ---
$localCobol = "COBOL\CALCDVOP.cbl"
$localComp = "JCL\COMPJCL.jcl"
$localRun = "JCL\RUNJCL.jcl"


# --- 1. SECURITY & CONFIG ---
#$USER_ID = $env:ZOWE_USER
#$PASSWORD = $env:ZOWE_PASSWORD

#if (-not $USER_ID -or -not $PASSWORD) {
#   Write-Error "CRITICAL: ZOWE_USER or ZOWE_PASSWORD not found in Environment Variables!"
#   exit
#}

$CBL_PDS = "$USER_ID.ZMYPRSNL.COBOL"
$JCL_PDS = "$USER_ID.ZMYPRSNL.JCL"
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
zowe files upload file-to-data-set $localCobol  "$CBL_PDS(CALCDVOP)" --user $env:ZOWE_USER_ID --pass $env:ZOWE_PASSWORD
if ($LASTEXITCODE -eq 0) {
   Write-Host "✅ SUCCESS: COBOL uploaded to PDS." -ForegroundColor Green
}
else {
   Write-Host "❌ ERROR: Upload failed. Check your credentials and paths." -ForegroundColor Red
   exit $LASTEXITCODE
}
zowe files upload file-to-data-set $localComp   "$JCL_PDS(COMPJCL)"  --user $env:ZOWE_USER_ID --pass $env:ZOWE_PASSWORD
zowe files upload file-to-data-set $localRun    "$JCL_PDS(RUNJCL)"   --user $env:ZOWE_USER_ID --pass $env:ZOWE_PASSWORD

# --- 5. COMPILE ---
Write-Log "[4/6] Submitting Compile Job..." "Yellow"
$compJob = zowe jobs submit data-set "$JCL_PDS(COMPJCL)" --wait-for-output --view-all-spool-content --user $env:ZOWE_USER_ID --pass $env:ZOWE_PASSWORD

if ($compJob -match "retcode: CC 0000") {
   Write-Log "SUCCESS: Compilation Clean." "Green"

   # --- 6. AUTO-TEST ---
   Write-Log "[5/6] Running Automated Unit Test..." "Yellow"
   $runJob = zowe jobs submit data-set "$JCL_PDS(RUNJCL)" --wait-for-output --view-all-spool-content --user $env:ZOWE_USER_ID --pass $env:ZOWE_PASSWORD

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