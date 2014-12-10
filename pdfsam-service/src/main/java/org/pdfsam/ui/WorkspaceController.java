/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 04/dic/2014
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
package org.pdfsam.ui;

import static java.util.Objects.nonNull;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.FileInputStream;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.Module;
import org.pdfsam.ui.workspace.LoadWorkspaceEvent;
import org.pdfsam.ui.workspace.SaveWorkspaceEvent;
import org.sejda.eventstudio.annotation.EventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.jr.ob.JSON;

/**
 * Controller for workspace related service functionalities
 * 
 * @author Andrea Vacondio
 *
 */
@Named
public class WorkspaceController {
    private static final Logger LOG = LoggerFactory.getLogger(WorkspaceController.class);

    private Map<String, Module> modulesMap;

    @Inject
    WorkspaceController(Map<String, Module> modulesMap) {
        this.modulesMap = modulesMap;
        eventStudio().addAnnotatedListeners(this);
    }

    @EventListener
    public CompletableFuture<Void> saveWorkspace(SaveWorkspaceEvent event) {
        LOG.debug(DefaultI18nContext.getInstance().i18n("Requesting modules state"));
        return CompletableFuture
                .allOf(modulesMap.values().stream()
                        .map(m -> CompletableFuture.runAsync(() -> eventStudio().broadcast(event, m.id())))
                        .toArray(CompletableFuture[]::new))
                .thenRun(
                        () -> {
                            LOG.debug(DefaultI18nContext.getInstance().i18n("Saving workspace data to {0}",
                                    event.getDestination().getAbsolutePath()));
                            try {
                                JSON.std.with(JSON.Feature.PRETTY_PRINT_OUTPUT)
                                        .without(JSON.Feature.WRITE_NULL_PROPERTIES)
                                        .write(event.getData(), event.getDestination());
                                LOG.info(DefaultI18nContext.getInstance().i18n("Workspace saved"));
                            } catch (Exception e1) {
                                LOG.error(DefaultI18nContext.getInstance().i18n("Unable to save modules workspace"), e1);
                            }
                        }).whenComplete((r, e) -> {
                    if (nonNull(e)) {
                        LOG.error(DefaultI18nContext.getInstance().i18n("Unable to save modules workspace"), e);
                    }
                });
    }

    @EventListener
    public CompletableFuture<Void> loadWorspace(LoadWorkspaceEvent event) {
        LOG.debug(DefaultI18nContext.getInstance().i18n("Loading workspace from {0}", event.workspace().getName()));
        return CompletableFuture
                .supplyAsync(
                        () -> {
                            Map<String, Object> data = Collections.emptyMap();
                            try (FileInputStream stream = new FileInputStream(event.workspace())) {
                                data = JSON.std.mapFrom(stream);
                            } catch (Exception e) {
                                LOG.error(
                                        DefaultI18nContext.getInstance().i18n("Unable to load workspace from {0}",
                                                event.workspace().getName()), e);
                            }
                            return data;
                        })
                .thenCompose(
                        (data) -> {
                            if (!data.isEmpty()) {
                                event.setData(data);
                                return CompletableFuture.allOf(modulesMap
                                        .values()
                                        .stream()
                                        .map(m -> CompletableFuture.runAsync(() -> eventStudio().broadcast(event,
                                                m.id()))).toArray(CompletableFuture[]::new));
                            }
                            return CompletableFuture.completedFuture(null);
                        })
                .whenComplete(
                        (r, e) -> {
                            if (nonNull(e)) {
                                LOG.error(
                                        DefaultI18nContext.getInstance().i18n("Unable to load workspace from {0}",
                                                event.workspace().getName()), e);
                            }
                        });

    }

}
