pipeline {
    agent any
    options {
        timeout(time: 3, unit: 'MINUTES') // Safety valve for Mainframe-Devops-Project
    }

    environment {
        // This ID must match the one you created in the Jenkins Credentials vault
        MF_CREDS = credentials('mainframe-auth')
        // This ID must match your GitHub token credential
        GIT_CREDS = credentials('github-auth')
    }

    stages {
        stage('Pull from GitHub') {
            steps {
                echo 'Pulling fresh Mainframe Automation Code...'
                git branch: 'main',
                    credentialsId: 'github-auth',
                    url: 'https://github.com/AtherShakeel/Mainframe-DevOps-Project'
            }
        }

        stage('Mainframe Automation') {
            steps {
                echo 'Starting Mainframe Deployment and Test...'
                // Jenkins fills %MF_CREDS_USR% and %MF_CREDS_PSW% automatically
                bat "python deploy-automation.py --user %MF_CREDS_USR% --passw %MF_CREDS_PSW%"
            }
        }
    }

    post {
        always {
            echo 'Pipeline finished. Check logs for details.'
        }
    }
}