/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08 nov 2016
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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
package org.pdfsam;

import org.pdfsam.module.ModuleServiceConfig;
import org.pdfsam.news.NewsServiceConfig;
import org.pdfsam.pdf.PdfServiceConfig;
import org.pdfsam.premium.PremiumServiceConfig;
import org.pdfsam.task.TaskExecutionServiceConfig;
import org.pdfsam.ui.UIServiceConfig;
import org.pdfsam.update.UpdateServiceConfig;
import org.sejda.injector.Injector;

/**
 * @author Andrea Vacondio
 *
 */
public class Services {
    /**
     * Adds configurations to the injector to create all the instances necessary to have the services up and running
     */
    public static void initServices() {
        Injector.addConfig(new UpdateServiceConfig(), new UIServiceConfig(), new TaskExecutionServiceConfig(),
                new PdfServiceConfig(), new NewsServiceConfig(), new ModuleServiceConfig(), new PremiumServiceConfig());
    }
}
