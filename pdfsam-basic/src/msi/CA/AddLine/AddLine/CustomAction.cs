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
                string path = session.CustomActionData["folder"] + "app/pdfsam.cfg";                
                if (File.Exists(@path)) 
                {
                    File.AppendAllText(path, "java-options=-Dorg.pdfsam.default.checkforupdate=" + session.CustomActionData["updates"] + Environment.NewLine);
                    File.AppendAllText(path, "java-options=-Dorg.pdfsam.default.donate.notification=" + session.CustomActionData["donate"] + Environment.NewLine);
                    File.AppendAllText(path, "java-options=-Dorg.pdfsam.default.locale=" + session.CustomActionData["locale"] + Environment.NewLine);
                    File.AppendAllText(path, "java-options=-Dorg.pdfsam.default.checkfornews=" + session.CustomActionData["news"] + Environment.NewLine);
                    File.AppendAllText(path, "java-options=-Dorg.pdfsam.default.play.sounds=" + session.CustomActionData["sounds"] + Environment.NewLine);
                    File.AppendAllText(path, "java-options=-Dorg.pdfsam.default.fetch.premium.modules=" + session.CustomActionData["premium"] + Environment.NewLine);
                    File.AppendAllText(path, "java-options=-Dorg.pdfsam.default.compression=" + session.CustomActionData["compression"] + Environment.NewLine);
                    File.AppendAllText(path, "java-options=-Dorg.pdfsam.default.output.overwrite=" + session.CustomActionData["overwrite"] + Environment.NewLine);
                    if (session.CustomActionData.ContainsKey("prefix")) {
                        string prefix = session.CustomActionData["prefix"];
                        if (!string.IsNullOrEmpty(prefix))
                        {
                            File.AppendAllText(path, "java-options=-Dorg.pdfsam.default.prefix=" + prefix + Environment.NewLine);
                        }
                    }
                    if (session.CustomActionData.ContainsKey("pdfversion")) {
                        string pdfversion = session.CustomActionData["pdfversion"];
                        if (!string.IsNullOrEmpty(pdfversion))
                        {
                            File.AppendAllText(path, "java-options=-Dorg.pdfsam.default.pdf.version=" + pdfversion + Environment.NewLine);
                        }
                    }
            }
            else   
                {
                    session.Log("Unable to find config file");
                }
	      }catch (Exception e){
                session.Log($"Exception: {e.Message}");
		        return ActionResult.Failure;
	      }
	      return ActionResult.Success;
        }
    }

