/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 04/dic/2014
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
package org.pdfsam.ui;

import static java.util.Objects.nonNull;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import javax.inject.Inject;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.injector.Auto;
import org.pdfsam.module.Module;
import org.pdfsam.ui.workspace.LoadWorkspaceEvent;
import org.pdfsam.ui.workspace.SaveWorkspaceEvent;
import org.pdfsam.ui.workspace.WorkspaceLoadedEvent;
import org.sejda.eventstudio.annotation.EventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Controller for workspace related service functionalities
 * 
 * @author Andrea Vacondio
 *
 */
@Auto
public class WorkspaceController {
    private static final Logger LOG = LoggerFactory.getLogger(WorkspaceController.class);

    private List<Module> modules;
    private WorkspaceService service;
    private RecentWorkspacesService recentWorkspace;

    @Inject
    WorkspaceController(List<Module> modules, WorkspaceService service, RecentWorkspacesService recentWorkspace) {
        this.modules = modules;
        this.service = service;
        this.recentWorkspace = recentWorkspace;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public void saveWorkspace(SaveWorkspaceEvent event) {
        LOG.debug(DefaultI18nContext.getInstance().i18n("Requesting modules state"));
        CompletableFuture<Void> future = CompletableFuture
                .allOf(modules.stream()
                        .map(m -> CompletableFuture.runAsync(() -> eventStudio().broadcast(event, m.id())))
                        .toArray(CompletableFuture[]::new))
                .thenRun(() -> service.saveWorkspace(event.getData(), event.workspace())).whenComplete((r, e) -> {
                    if (nonNull(e)) {
                        LOG.error(DefaultI18nContext.getInstance().i18n("Unable to save modules workspace"), e);
                    }
                });
        if (event.awaitCompletion) {
            try {
                future.get();
            } catch (InterruptedException | ExecutionException e) {
                LOG.error(DefaultI18nContext.getInstance().i18n("Unable to save workspace to {0}",
                        event.workspace().getName()), e);
            }
        }
    }

    @EventListener
    public CompletableFuture<Void> loadWorspace(LoadWorkspaceEvent event) {
        LOG.debug(DefaultI18nContext.getInstance().i18n("Loading workspace from {0}", event.workspace().getName()));
        return CompletableFuture.supplyAsync(() -> service.loadWorkspace(event.workspace())).thenCompose((data) -> {
            if (!data.isEmpty()) {
                event.setData(data);
                return CompletableFuture.allOf(modules.stream()
                        .map(m -> CompletableFuture.runAsync(() -> eventStudio().broadcast(event, m.id())))
                        .toArray(CompletableFuture[]::new)).thenRun(() -> {
                            recentWorkspace.addWorkspaceLastUsed(event.workspace());
                            eventStudio().broadcast(new WorkspaceLoadedEvent(event.workspace()));
                            LOG.info(DefaultI18nContext.getInstance().i18n("Workspace loaded"));
                        });
            }
            return CompletableFuture.completedFuture(null);
        }).whenComplete((r, e) -> {
            if (nonNull(e)) {
                LOG.error(DefaultI18nContext.getInstance().i18n("Unable to load workspace from {0}",
                        event.workspace().getName()), e);
            }
        });

    }
}
