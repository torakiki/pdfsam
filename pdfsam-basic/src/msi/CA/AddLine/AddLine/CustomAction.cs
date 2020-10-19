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
                    File.AppendAllText(path, "-Dorg.pdfsam.default.donate.notification=" + session.CustomActionData["donate"] + Environment.NewLine);
                    session.Log("Added -Dorg.pdfsam.default.donate.notification=" + session.CustomActionData["donate"]);
                    File.AppendAllText(path, "-Dorg.pdfsam.default.locale=" + session.CustomActionData["locale"] + Environment.NewLine);
                    session.Log("Added -Dorg.pdfsam.default.locale=" + session.CustomActionData["locale"]);                     
                    File.AppendAllText(path, "-Dorg.pdfsam.default.checkfornews=" + session.CustomActionData["news"] + Environment.NewLine);
                    session.Log("Added -Dorg.pdfsam.default.checkfornews=" + session.CustomActionData["news"]);                     
                    File.AppendAllText(path, "-Dorg.pdfsam.default.play.sounds=" + session.CustomActionData["sounds"] + Environment.NewLine);
                    session.Log("Added -Dorg.pdfsam.default.play.sounds=" + session.CustomActionData["sounds"]);                     
                    File.AppendAllText(path, "-Dorg.pdfsam.default.fetch.premium.modules=" + session.CustomActionData["premium"] + Environment.NewLine);
                    session.Log("Added -Dorg.pdfsam.default.fetch.premium.modules=" + session.CustomActionData["premium"]);
                    File.AppendAllText(path, "-Dorg.pdfsam.default.compression=" + session.CustomActionData["compression"] + Environment.NewLine);
                    session.Log("Added -Dorg.pdfsam.default.compression=" + session.CustomActionData["compression"]);
                    File.AppendAllText(path, "-Dorg.pdfsam.default.output.overwrite=" + session.CustomActionData["overwrite"] + Environment.NewLine);
                    session.Log("Added -Dorg.pdfsam.default.output.overwrite=" + session.CustomActionData["overwrite"]);                      
                }else   
                {
                    session.Log("Unable to find l4j ini file");
                }
	      }catch (Exception){
		        return ActionResult.Failure;
	      }
	      return ActionResult.Success;
        }
    }

