# ==============================================================================
# VibeGarden Master Pipeline: Sync-Gated Deployment
# ==============================================================================

# ==============================================================================
# 1. PRE-FLIGHT DEPENDENCY CHECK
# ==============================================================================
$dependenciesPassed = $true

# A. Check for Git
if (-not (Get-Command git -ErrorAction SilentlyContinue)) {
    Write-Host "[ERROR] Git is NOT installed. Please install Git for Windows." -ForegroundColor Red
    $dependenciesPassed = $false
}

# B. Check for Zowe CLI
if (-not (Get-Command zowe -ErrorAction SilentlyContinue)) {
    Write-Host "[ERROR] Zowe CLI is NOT installed. Please install it via Node.js (npm install -g @zowe/cli)." -ForegroundColor Red
    $dependenciesPassed = $false
}

# C. Check for Environment Variables (The Credentials)
if ([string]::IsNullOrWhiteSpace($env:ZOWE_USER) -or [string]::IsNullOrWhiteSpace($env:ZOWE_PASSWORD)) {
    Write-Host "[ERROR] Missing Credentials: ZOWE_USER and ZOWE_PASSWORD environment variables must be set." -ForegroundColor Red
    Write-Host "   Try: $env:ZOWE_USER='your_id'; $env:ZOWE_PASSWORD='your_password'" -ForegroundColor Gray
    $dependenciesPassed = $false
}

# D. Check Folder Structure
$requiredFolders = @("COBOL", "JCL")
foreach ($folder in $requiredFolders) {
    if (-not (Test-Path $folder)) {
        Write-Host "[ERROR] Missing Folder: Could not find the '$folder' directory in the current path." -ForegroundColor Red
        $dependenciesPassed = $false
    }
}

# E. Final Decision: Stop if any check failed
if (-not $dependenciesPassed) {
    Write-Host "`n VibeGarden Pipeline cannot start until dependencies are fixed.`n" -ForegroundColor Yellow
    exit 1
}

Write-Host " All dependencies verified. Starting VibeGarden Master Pipeline..." -ForegroundColor Cyan

# ==============================================================================
# 2. CONFIGURATION ---
# ==============================================================================
$myUSER_ID = $env:ZOWE_USER
$myPASSWORD = $env:ZOWE_PASSWORD
$JCL_PDS = "$($myUSER_ID).ZMYPRSNL.JCL"
$COBOL_PDS = "$($myUSER_ID).ZMYPRSNL.COBOL"

# A. Log File Setup
$logDir = "./logs"
if (-not (Test-Path $logDir)) { New-Item -Path $logDir -ItemType Directory }
$logFile = "$logDir/deploy-$(Get-Date -Format 'yyyyMMdd-HHmm').log"

# B. Write logs into the new log file
function Write-Log($msg, $color) {
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    Write-Host "`n$msg" -ForegroundColor $color
    Write-Host "`n****************************************"

    # C. # Still write the plain text to the log file
    "[$timestamp] $($msg.Trim())" | Out-File -FilePath $logFile -Append
}

#==============================================================================
# 3. Start the stopwatch
#==============================================================================
$totalStartTime = Get-Date

#==============================================================================
# 4. PRE-DEPLOYMENT GIT RECORD
# We commit locally first so your work is saved before we talk to the mainframe.
#==============================================================================
Write-Log "[1/7] Recording local changes (Local Commit)..." "Yellow"
git add .
git commit -m "VibeGarden Build Started: $(Get-Date -Format 'HH:mm')" --allow-empty

#==============================================================================
# 5. VERSIONING LOGIC
#==============================================================================
$version = Get-Date -Format "yyyyMMdd"
$buildId = "B" + (Get-Date -Format "HHmm") # Example: B1430
$newTag = "$version-$buildId"

Write-Log " Updating COBOL Version Tag to: $newTag" "Cyan"

# Read the file, replace the placeholder, and save it back
(Get-Content "COBOL\CALCDVOP.cbl") -replace "BUILD-TAG", $newTag |
Set-Content "COBOL\CALCDVOP.cbl"

#==============================================================================
# 6. MAINFRAME UPLOAD
#==============================================================================
Write-Log "[2/7] Uploading Source to $myUSER_ID..." "Yellow"
zowe files upload file-to-data-set "COBOL\CALCDVOP.cbl" "$COBOL_PDS(CALCDVOP)" --user $myUSER_ID --pass $myPASSWORD
zowe files upload file-to-data-set "JCL\COMPJCL.jcl"    "$JCL_PDS(COMPJCL)"    --user $myUSER_ID --pass $myPASSWORD
zowe files upload file-to-data-set "JCL\RUNJCL.jcl"     "$JCL_PDS(RUNJCL)"     --user $myUSER_ID --pass $myPASSWORD

#==============================================================================
# 7. COMPILE SECTION
#==============================================================================
Write-Log "[3/7] Compiling COBOL..." "Yellow"
$compRaw = zowe jobs submit data-set "$JCL_PDS(COMPJCL)" --wait-for-output --rfj --user $myUSER_ID --pass $myPASSWORD
$compJob = $compRaw | ConvertFrom-Json
$rc = $compJob.data.retcode
# A. If $rc is empty, it means the Zowe command failed to get a result at all
if ([string]::IsNullOrWhiteSpace($rc)) {
    $rc = "UNKNOWN (Zowe Communication Error)"
}

if ($rc -ne "CC 0000" -and $rc -ne "CC 0004") {
    Write-Log "[ERROR] COMPILE FAILED: $($rc). Check spool for errors." "Red"
    exit 1 # Script stops here; nothing is pushed to GitHub.
}
Write-Log " COMPILE SUCCESS" "Green"

#==============================================================================
# 8. EXECUTION SECTION
#==============================================================================
Write-Log "[4/7] Running Automated Test (RUNJCL)..." "Yellow"
$runRaw = zowe jobs submit data-set "$JCL_PDS(RUNJCL)" --wait-for-output --rfj --user $myUSER_ID --pass $myPASSWORD
$runJob = $runRaw | ConvertFrom-Json
$jobId = $runJob.data.jobid

# A. Dynamic Spool Lookup for SYSOUT
$spoolFiles = zowe jobs list spool-files-by-jobid $jobId --rfj --user $myUSER_ID --pass $myPASSWORD | ConvertFrom-Json
$sysoutId = ($spoolFiles.data | Where-Object { $_.ddname.Trim() -eq "SYSOUT" }).id
if (-not $sysoutId) { $sysoutId = $spoolFiles.data[-1].id } # Fallback to last file

$testResults = zowe jobs view spool-file-by-id $jobId $sysoutId --user $myUSER_ID --pass $myPASSWORD

#==============================================================================
# 9. VALIDATION & GITHUB PUSH
#==============================================================================
Write-Log "[5/7] Validating Result Logic..." "Yellow"

Write-Host "--- PROGRAM OUTPUT ---" -ForegroundColor Gray
$testResults.Trim()
Write-Host "----------------------" -ForegroundColor Gray

# A. Regex captures the decimal/number even with commas
if ($testResults -match "VibeGarden Result:\s+(?<val>[\d,.]+)") {
    $foundValue = $Matches['val'].Trim()
    Write-Log " TEST PASSED: Captured Result: $foundValue" "Green"

    # SUCCESS GATE: Only push to GitHub if we reached this line
    Write-Log "[6/7] Success! Pushing 'Blessed' code to GitHub..." "Yellow"
    git commit --amend -m "VibeGarden Success: Job $jobId - Result $foundValue"
    git push
    Write-Log " GITHUB SYNCED" "Cyan"
}
else {
    Write-Log "[ERROR] TEST FAILED: Result mismatch." "Red"
    Write-Log " [RUNTIME ERROR] WARNING: Code is updated on Mainframe but NOT pushed to GitHub (Fix the error first)." "Yellow"
    exit 1
}

#==============================================================================
# 10. Calculate elapsed time and write the final completion message to the log
#==============================================================================
$totalEndTime = Get-Date
$duration = $totalEndTime - $totalStartTime
$timeString = "{0:mm} min {0:ss} sec" -f $duration

Write-Log "[7/7] VibeGarden Pipeline Finished Successfully in $timeString!" "Magenta"

#===========================================================================================================================================s