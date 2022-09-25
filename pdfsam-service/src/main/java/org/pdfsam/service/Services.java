/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08 nov 2016
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
package org.pdfsam.service;

import org.pdfsam.injector.Injector;
import org.pdfsam.service.news.NewsServiceConfig;
import org.pdfsam.service.pdf.PdfServiceConfig;
import org.pdfsam.service.premium.PremiumServiceConfig;
import org.pdfsam.service.task.TaskExecutionServiceConfig;
import org.pdfsam.service.tool.ModuleServiceConfig;
import org.pdfsam.service.ui.UIServiceConfig;
import org.pdfsam.service.update.UpdateServiceConfig;

/**
 * @author Andrea Vacondio
 *
 */
public class Services {
    /**
     * Adds configurations to the injector to create all the instances necessary to have the services up and running
     */
    public static void initServices() {
        Injector.addConfig(new ServicesConfig(), new UpdateServiceConfig(), new UIServiceConfig(),
                new TaskExecutionServiceConfig(), new PdfServiceConfig(), new NewsServiceConfig(),
                new ModuleServiceConfig(), new PremiumServiceConfig());
    }
}
