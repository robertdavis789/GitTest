#!/usr/bin/env groovy
import hudson.model.*
import hudson.EnvVars
import groovy.json.JsonSlurper
import groovy.json.JsonBuilder
import groovy.json.JsonOutput
import java.net.URL


 String Git_Credentials      = "GitHub"
 String Git_URL              = "https://github.com/robertdavis789/GitTest.git"
 String ISPW_Application     = "PLAY"
 String HCI_Conn_ID	         = "fc29cb3e-b2f9-4573-ae44-4a6a201c8e07"
 String HCI_Token            = "Topaz"
 String ISPW_Stream	         = "PLAY"
 String CES_TOKEN            = "6af7954f-3954-4b78-a339-286e6967742d"
 String LEVEL
 String ASSIGNMENT


/* 
  Node is a required part of the Jenkins pipeline for any steps to be executed
*/ 
node{

	/*  This stage will retrieve the code from Git   */
    stage("Checkout")
    {
      checkout scm
    }    
    
    /*  This loads the changed files into ISPW    */ 
    stage("Loading Mainframe code to ISPW") 
    {
            gitToIspwIntegration app: "${ISPW_Application}", 
            branchMapping: '''*dev2* => DEV2, per-branch''',
            connectionId: "${HCI_Conn_ID}", 
            credentialsId: "${HCI_Token}", 
            gitCredentialsId: "${Git_Credentials}", 
            gitRepoUrl: "${Git_URL}", 
            runtimeConfig: 'tpzp', 
            stream: "${ISPW_Stream}"
 
    }

    stage("Build Mainframe Code")
    {
                
        ispwOperation connectionId: "${HCI_Conn_ID}", 
        consoleLogResponseBody: false, 
        credentialsId: "${CES_TOKEN}", 
        ispwAction: 'BuildTask',
        ispwRequestBody: '''buildautomatically = true'''
    
    }
    
    stage("Deploy Mainframe Code")
    {
    
        sleep 3
        def automaticBuildParams = readJSON file: 'automaticBuildParams.txt'
   
        ispwOperation connectionId: "${HCI_Conn_ID}", 
        consoleLogResponseBody: false, 
        credentialsId: "${CES_TOKEN}", 
        ispwAction: 'DeployAssignment', 
        ispwRequestBody: "assignmentId=${automaticBuildParams.containerId}\nlevel=${automaticBuildParams.taskLevel}" 
    
    }
    stage("SonarQube Scan")
    {
        // Requires SonarQube Scanner 2.8+
        def scannerHome = tool 'scanner';
        withSonarQubeEnv('cwcc') 
        {
            def SQ_PullRequest          = " -Dsonar.branch.name=${env.BRANCH_NAME} -Dsonar.branch.target=master"
            def SQ_ProjectKey           = " -Dsonar.projectKey=SXK1_Git -Dsonar.projectName=GIT1 -Dsonar.projectVersion=1.0"
            def SQ_Source               = " -Dsonar.sources=Mainframe/Cobol"
            def SQ_Copybook             = " -Dsonar.cobol.copy.directories=Mainframe/Cobol/Copybooks"
            def SQ_Cobol_conf           = " -Dsonar.cobol.file.suffixes=cbl,testsuite,testscenario,stub -Dsonar.cobol.copy.suffixes=cpy -Dsonar.sourceEncoding=UTF-8"
 
            if (env.BRANCH_NAME == 'master') {
                bat "${scannerHome}/bin/sonar-scanner" + SQ_ProjectKey + SQ_Source + SQ_Copybook + SQ_Cobol_conf
            } else {
                bat "${scannerHome}/bin/sonar-scanner" + SQ_PullRequest + SQ_ProjectKey + SQ_Source + SQ_Copybook + SQ_Cobol_conf        
            }
            
        }
    }
}