#!/usr/bin/env groovy
import hudson.model.*
import hudson.EnvVars
import groovy.json.JsonSlurper
import groovy.json.JsonBuilder
import groovy.json.JsonOutput
import java.net.URL


 String Git_Credentials      = "a54eaaca-e673-4912-9500-a593533f6bf9"
 String Git_URL              = "https://github.com/robertdavis789/GitTest.git"
 String ISPW_Application     = "PLAY"
 String HCI_Conn_ID	         = "cw09"
 String HCI_Token            = "b3a06d76-c084-4255-8ab9-f41b48e33b81"
 String ISPW_Stream	         = "PLAY"
 String CES_TOKEN            = "1a256d6e-7f5b-406a-a9e4-061e77a60f11"		 //6839356e-0256-4a8f-8de9-3d223a1b7d36
 String LEVEL                = "DEV1"        


 def String DetermineLevel() {
 
        def String Request = "assignmentId=GIT1000007\nlevel=HFIX"

        if (env.BRANCH_NAME == 'master'){
            Request = "assignmentId=GIT1000009\n" + "level=MSTR"
        }
        else if(env.BRANCH_NAME == 'feature1'){
            Request = "assignmentId=GIT1000008\n" + "level=FT1"
        }
        else if (env.BRANCH_NAME == 'feature2'){
            Request = "assignmentId=GIT1000008\n" + "level=FT2"
        }       
        else if(env.BRANCH_NAME == 'hotfix'){
            Request = "assignmentId=GIT1000007\n" + "level=HFIX"
        } 
        return Request
}

/* 
  Node is a required part of the Jenkins pipeline for any steps to be executed
*/ 
node{

    stage("Checkout")
    {
      checkout scm
    }    

    /*  This loads the changed files into ISPW    */ 
    stage("Loading Mainframe code to ISPW") 
    {
    /*
            gitToIspwIntegration app: "${ISPW_Application}", 
            branchMapping: '''*master* => DEV1, per-branch
            *feature1* => DEV1, per-branch
            *ug* => DEV1, per-branch''', 
            connectionId: "${HCI_Conn_ID}", 
            credentialsId: "${HCI_Token}", 
            gitCredentialsId: "${Git_Credentials}", 
            gitRepoUrl: "${Git_URL}", 
            runtimeConfig: '', 
            stream: "${ISPW_Stream}"*/
 
	gitToIspwIntegration app: 'PLAY', 
	branchMapping: '''*master* => DEV9, per-commit''', 
	connectionId: 'cd495bfe-0ddb-48d2-b5af-2bf43e443ff7', 
	credentialsId: '897ae347-b055-495d-9c9a-335cf9b5791a', 
	gitCredentialsId: 'a54eaaca-e673-4912-9500-a593533f6bf9', 
	gitRepoUrl: 'https://github.com/robertdavis789/GitTest.git', 
	runtimeConfig: 'TPZP', 
	stream: 'PLAY'
 
 
    }

/*
    stage("Build Mainframe Code")
    {
        LEVEL = DetermineLevel()
        
        echo "${CES_TOKEN}"
        
        ispwOperation connectionId: "${HCI_Conn_ID}", 
        consoleLogResponseBody: true, 
        credentialsId: "${CES_TOKEN}", 
        ispwAction: 'GenerateTasksInAssignment', 
        ispwRequestBody: LEVEL
        //sleep 3
    }
    
    stage("Deploy Mainframe Code")
    {
        //LEVEL = DetermineLevel()
        
        //Request = "assignmentId=GIT1000003\n" + "level=${LEVEL}"    

        //ispwOperation connectionId: "${HCI_Conn_ID}", 
        //consoleLogResponseBody: true, 
        //credentialsId: "${CES_TOKEN}", 
        //ispwAction: 'DeployAssignment', 
        //ispwRequestBody: LEVEL 
        sleep 4
    }
    */

}