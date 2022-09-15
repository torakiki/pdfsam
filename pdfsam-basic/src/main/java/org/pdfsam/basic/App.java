/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ott/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.basic;

import org.pdfsam.PdfsamApp;
import org.pdfsam.alternatemix.AlternateMixTool;
import org.pdfsam.extract.ExtractTool;
import org.pdfsam.injector.Injector;

import javafx.application.Application;
import org.pdfsam.merge.MergeTool;
import org.pdfsam.rotate.RotateTool;
import org.pdfsam.split.SplitTool;
import org.pdfsam.splitbybookmarks.SplitByBookmarksTool;
import org.pdfsam.splitbysize.SplitBySizeTool;

/**
 * PDFsam Basic Edition App
 * 
 * @author Andrea Vacondio
 *
 */
public class App {
    public static void main(String[] args) {
        Injector.addConfig(new PdfsamBasicConfig(), new AlternateMixTool.ModuleConfig(),
                new ExtractTool.ModuleConfig(), new MergeTool.ModuleConfig(),
                new RotateTool.ModuleConfig(), new SplitTool.ModuleConfig(),
                new SplitByBookmarksTool.ModuleConfig(),
                new SplitBySizeTool.ModuleConfig());
        Application.launch(PdfsamApp.class, args);
    }

}
