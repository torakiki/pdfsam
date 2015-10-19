using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Deployment.WindowsInstaller;
using System.IO;

    public class CustomActions
    {
        [CustomAction]
        public static ActionResult AddLine(Session session)
        {
          try{
                string path = session.CustomActionData["folder"] + "pdfsam.l4j.ini";                
                if (File.Exists(@path)) 
                {
                    session.Log("AddLine to existing " + session.CustomActionData["folder"] + "pdfsam.l4j.ini");
                    File.AppendAllText(path, Environment.NewLine + "-Dorg.pdfsam.default.checkforupdate=" + session.CustomActionData["updates"] + Environment.NewLine);
                    session.Log("Added -Dorg.pdfsam.default.checkforupdate=" + session.CustomActionData["updates"]);
                    File.AppendAllText(path, "-Dorg.pdfsam.default.locale=" + session.CustomActionData["locale"] + Environment.NewLine);
                    session.Log("Added -Dorg.pdfsam.default.locale=" + session.CustomActionData["locale"]);                     
                }else   
                {
                    session.Log("Uable to find l4j ini file");
                }
	      }catch (Exception){
		        return ActionResult.Failure;
	      }
	      return ActionResult.Success;
        }
    }

