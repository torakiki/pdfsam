package org.pdfsam.gui.configuration;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 06/10/22
 * Copyright 2022 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import org.pdfsam.injector.Components;
import org.pdfsam.injector.Provides;
import org.pdfsam.service.news.DefaultNewsService;
import org.pdfsam.service.news.LatestNewsController;
import org.pdfsam.service.news.NewsService;
import org.pdfsam.service.pdf.BookmarksLevelSAMBoxLoader;
import org.pdfsam.service.pdf.DefaultPdfLoadService;
import org.pdfsam.service.pdf.DefaultSAMBoxLoader;
import org.pdfsam.service.pdf.PdfLoadController;
import org.pdfsam.service.pdf.PdfLoadService;
import org.pdfsam.service.premium.DefaultPremiumToolsService;
import org.pdfsam.service.premium.PremiumToolsController;
import org.pdfsam.service.premium.PremiumToolsService;
import org.pdfsam.service.task.TaskExecutionController;
import org.pdfsam.service.tool.DefaultUsageService;
import org.pdfsam.service.tool.UsageService;
import org.pdfsam.service.ui.DefaultRecentWorkspacesService;
import org.pdfsam.service.ui.DefaultStageService;
import org.pdfsam.service.ui.DefaultWorkspaceService;
import org.pdfsam.service.ui.RecentWorkspacesService;
import org.pdfsam.service.ui.StageService;
import org.pdfsam.service.ui.StageServiceController;
import org.pdfsam.service.ui.WorkspaceController;
import org.pdfsam.service.ui.WorkspaceService;
import org.pdfsam.service.update.DefaultUpdateService;
import org.pdfsam.service.update.UpdateService;
import org.pdfsam.service.update.UpdatesController;
import org.sejda.core.service.DefaultTaskExecutionService;
import org.sejda.core.service.TaskExecutionService;

import java.util.Arrays;

/**
 * @author Andrea Vacondio
 */
@Components({ LatestNewsController.class, PdfLoadController.class, PremiumToolsController.class,
        TaskExecutionController.class, WorkspaceController.class, StageServiceController.class,
        UpdatesController.class })
public class ServicesConfig {

    @Provides
    NewsService news(DefaultNewsService news) {
        return news;
    }

    @Provides
    PdfLoadService loadService() {
        return new DefaultPdfLoadService(Arrays.asList(new DefaultSAMBoxLoader(), new BookmarksLevelSAMBoxLoader()));
    }

    @Provides
    PremiumToolsService premiumTools(DefaultPremiumToolsService service) {
        return service;
    }

    @Provides
    TaskExecutionService executionService() {
        return new DefaultTaskExecutionService();
    }

    @Provides
    UsageService news(DefaultUsageService service) {
        return service;
    }

    @Provides
    RecentWorkspacesService recentWorkspaces(DefaultRecentWorkspacesService service) {
        return service;
    }

    @Provides
    WorkspaceService workspaces(DefaultWorkspaceService service) {
        return service;
    }

    @Provides
    StageService stage(DefaultStageService service) {
        return service;
    }

    @Provides
    UpdateService news(DefaultUpdateService updates) {
        return updates;
    }
}
