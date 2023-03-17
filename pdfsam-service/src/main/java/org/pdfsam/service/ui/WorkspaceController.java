/*
 * This file is part of the PDF Split And Merge source code
 * Created on 04/dic/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
import org.pdfsam.model.ui.workspace.WorkspaceLoadedEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static java.util.Objects.nonNull;
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
        LOG.debug(i18n().tr("Requesting modules state"));
        CompletableFuture<Void> future = CompletableFuture.allOf(
                        tools.stream().map(m -> CompletableFuture.runAsync(() -> eventStudio().broadcast(event, m.id())))
                                .toArray(CompletableFuture[]::new))
                .thenRun(() -> service.saveWorkspace(event.data(), event.workspace())).whenComplete((r, e) -> {
                    if (nonNull(e)) {
                        LOG.error(i18n().tr("Unable to save workspace to {0}", event.workspace().getName()), e);
                    }
                });
        if (event.awaitCompletion()) {
            try {
                future.get();
            } catch (InterruptedException | ExecutionException e) {
                LOG.error(i18n().tr("Unable to save workspace to {0}", event.workspace().getName()), e);
            }
        }
    }

    @EventListener
    public CompletableFuture<Void> loadWorspace(LoadWorkspaceRequest event) {
        LOG.debug(i18n().tr("Loading workspace from {0}", event.workspace().getName()));
        return CompletableFuture.supplyAsync(() -> service.loadWorkspace(event.workspace())).thenCompose((data) -> {
            if (!data.isEmpty()) {
                var response = new LoadWorkspaceResponse(event.workspace(), data);
                return CompletableFuture.allOf(tools.stream()
                        .map(m -> CompletableFuture.runAsync(() -> eventStudio().broadcast(response, m.id())))
                        .toArray(CompletableFuture[]::new)).thenRun(() -> {
                    recentWorkspace.addWorkspaceLastUsed(event.workspace());
                    eventStudio().broadcast(new WorkspaceLoadedEvent(event.workspace()));
                    LOG.info(i18n().tr("Workspace loaded: {0}", event.workspace().getName()));
                });
            }
            return CompletableFuture.completedFuture(null);
        }).whenComplete((r, e) -> {
            if (nonNull(e)) {
                LOG.error(i18n().tr("Unable to load workspace from {0}", event.workspace().getName()), e);
            }
        });

    }
}
