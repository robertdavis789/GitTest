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
    gitCredentialsId: 'Bitbucket', 
    gitRepoUrl: 'https://robert.davis%40compuware.com@evolve.compuware.com/scm/~robert.davis_compuware.com/gitrepo.git', 
    runtimeConfig: 'TPZP',
    stream: 'PLAY'
 }
}