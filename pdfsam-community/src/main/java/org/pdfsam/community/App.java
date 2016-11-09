/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ott/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.community;

import org.pdfsam.PdfsamApp;
import org.sejda.injector.Injector;

import javafx.application.Application;

/**
 * PDFsam Community Edition App
 * 
 * @author Andrea Vacondio
 *
 */
public class App {
    public static void main(String[] args) {
        Injector.addConfig(new PdfsamCommunityConfig(), new org.pdfsam.alternatemix.AlternateMixModule.ModuleConfig(),
                new org.pdfsam.extract.ExtractModule.ModuleConfig(), new org.pdfsam.merge.MergeModule.ModuleConfig(),
                new org.pdfsam.rotate.RotateModule.ModuleConfig(), new org.pdfsam.split.SplitModule.ModuleConfig(),
                new org.pdfsam.splitbybookmarks.SplitByBookmarksModule.ModuleConfig(),
                new org.pdfsam.splitbysize.SplitBySizeModule.ModuleConfig());
        Application.launch(PdfsamApp.class, args);
    }

}
