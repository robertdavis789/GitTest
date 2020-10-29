node {
 stage ('Checkout') 
 {
    // Get the code from the repository
    // Test4
    checkout scm
 }
 
 stage('Git to ISPW Synchronization')
 { 
    gitToIspwIntegration app: 'PLAY', 
    branchMapping: '''*dev2* => DEV2, per-branch''',
    connectionId: 'fc29cb3e-b2f9-4573-ae44-4a6a201c8e07', 
    credentialsId: 'Topaz', 
    gitCredentialsId: 'GitHub', 
    gitRepoUrl: 'https://github.com/robertdavis789/GitTest.git', 
    runtimeConfig: 'TPZP',
    stream: 'PLAY'
 }
 stage ('Build ISPW task')
 {
    ispwOperation connectionId: 'fc29cb3e-b2f9-4573-ae44-4a6a201c8e0',
	consoleLogResponseBody: true,
	credentialsId: 'CES',
	ispwAction: 'BuildAssignment',
	ispwRequestBody: '''buildAutomatically=true'''
 }
}