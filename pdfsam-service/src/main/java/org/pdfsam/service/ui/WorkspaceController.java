/*
 * This file is part of the PDF Split And Merge source code
 * Created on 04/dic/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.service.ui;

import jakarta.inject.Inject;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.injector.Auto;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.ui.workspace.LoadWorkspaceRequest;
import org.pdfsam.model.ui.workspace.LoadWorkspaceResponse;
import org.pdfsam.model.ui.workspace.SaveWorkspaceRequest;
import org.pdfsam.model.ui.workspace.WorkspaceData;
import org.pdfsam.model.ui.workspace.WorkspaceLoadedEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.concurrent.StructuredTaskScope;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Controller for workspace related service functionalities
 *
 * @author Andrea Vacondio
 */
@Auto
public class WorkspaceController {
    private static final Logger LOG = LoggerFactory.getLogger(WorkspaceController.class);

    private final Collection<Tool> tools;
    private final WorkspaceService service;
    private final RecentWorkspacesService recentWorkspace;
    private WorkspaceData workspace;

    @Inject
    WorkspaceController(WorkspaceService service, RecentWorkspacesService recentWorkspace) {
        this(app().runtimeState().tools().values(), service, recentWorkspace);
    }

    WorkspaceController(Collection<Tool> tools, WorkspaceService service, RecentWorkspacesService recentWorkspace) {
        this.tools = tools;
        this.service = service;
        this.recentWorkspace = recentWorkspace;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void saveWorkspace(SaveWorkspaceRequest event) {
        Thread.ofVirtual().name("save-workspace-thread").start(() -> {
            try (var scope = new StructuredTaskScope.ShutdownOnFailure()) {
                LOG.debug(i18n().tr("Requesting modules state"));
                tools.forEach(m -> scope.fork(() -> {
                    eventStudio().broadcast(event, m.id());
                    return null;
                }));
                scope.join();
                scope.throwIfFailed();
                service.saveWorkspace(event.data(), event.workspace());
            } catch (Exception e) {
                LOG.error(i18n().tr("Unable to save workspace to {0}", event.workspace().getName()), e);
            }
        });
    }

    @EventListener
    public void loadWorkspace(LoadWorkspaceRequest event) {
        Thread.ofVirtual().name("load-workspace-thread").start(() -> {
            LOG.debug(i18n().tr("Loading workspace from {0}", event.workspace().getName()));
            try (var scope = new StructuredTaskScope.ShutdownOnFailure()) {
                var data = service.loadWorkspace(event.workspace());
                workspace = new WorkspaceData(event.workspace(), data);
                if (!data.isEmpty()) {
                    var response = new LoadWorkspaceResponse(workspace);
                    tools.forEach(m -> scope.fork(() -> {
                        eventStudio().broadcast(response, m.id());
                        return null;
                    }));
                    scope.join();
                    scope.throwIfFailed();
                    recentWorkspace.addWorkspaceLastUsed(event.workspace());
                    eventStudio().broadcast(new WorkspaceLoadedEvent(event.workspace()));
                    LOG.info(i18n().tr("Workspace loaded: {0}", event.workspace().getName()));
                }
            } catch (Exception e) {
                LOG.error(i18n().tr("Unable to load workspace from {0}", event.workspace().getName()), e);
            }
        });
    }
}
