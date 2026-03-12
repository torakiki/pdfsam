/*
 * This file is part of the PDF Split And Merge source code
 * Created on 21/mar/2014
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
package org.pdfsam.ui.components.tool;

import javafx.scene.control.MenuItem;
import javafx.scene.control.SplitMenuButton;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.pdfsam.eventstudio.ReferenceStrength;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.pdfsam.model.io.NativeOpenFileRequest;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.model.tool.ClearToolRequest;
import org.pdfsam.model.tool.TaskExecutionRequest;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.tool.ToolInputOutputType;
import org.pdfsam.model.ui.SetActiveContentItemRequest;
import org.pdfsam.ui.components.support.Style;
import org.sejda.model.exception.TaskOutputVisitException;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.output.DirectoryTaskOutput;
import org.sejda.model.output.FileOrDirectoryTaskOutput;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.output.TaskOutputDispatcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static java.util.Objects.isNull;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Button to open the latest manipulation result
 *
 * @author Andrea Vacondio
 */
public class OpenButton extends SplitMenuButton implements TaskOutputDispatcher {

    private static final Logger LOG = LoggerFactory.getLogger(OpenButton.class);
    private final String ownerModule;
    private File destination;
    private final List<File> latestOutput = new ArrayList<>();

    public OpenButton(String ownerTool, ToolInputOutputType outputType) {
        this(ownerTool, outputType, app().runtimeState().tools().values());
    }

    OpenButton(String ownerModule, ToolInputOutputType outputType, Collection<Tool> tools) {
        this.ownerModule = defaultString(ownerModule);
        setId(ownerModule + ".openButton");
        getStyleClass().addAll(Style.BUTTON.css());
        getStyleClass().addAll("pdfsam-split-button", "footer-open-button");
        setText(i18n().tr("Open"));
        setMaxHeight(Double.MAX_VALUE);
        setPrefHeight(Double.MAX_VALUE);
        setVisible(false);
        setOnAction(e -> {
            if (latestOutput.size() != 1 || !openFile(latestOutput.getFirst())) {
                openFile(destination);
            }

        });
        eventStudio().add(TaskExecutionRequest.class, e -> {
            if (e.toolId().equals(ownerModule)) {
                latestOutput.clear();
                try {
                    if (!isNull(e.parameters().getOutput())) {
                        e.parameters().getOutput().accept(this);
                    }
                } catch (TaskOutputVisitException ex) {
                    LOG.warn("This should never happen", ex);
                }
            }
        }, -10, ReferenceStrength.STRONG);
        tools.forEach(m -> {
            if (m.descriptor().hasInputType(outputType)) {
                getItems().add(new OpenWithMenuItem(m));
            }
        });
        eventStudio().addAnnotatedListeners(this);
    }

    private boolean openFile(File file) {
        if (file != null && file.exists()) {
            eventStudio().broadcast(new NativeOpenFileRequest(file));
            return true;
        }
        return false;
    }

    @EventStation
    public String getOwnerModule() {
        return ownerModule;
    }

    @EventListener(priority = -10)
    public void onTaskCompleted(TaskExecutionCompletedEvent event) {
        latestOutput.addAll(event.getNotifiableTaskMetadata().taskOutput());
    }

    @Override
    public void dispatch(FileTaskOutput output) {
        destination = output.getDestination();
        setAccessibleText(i18n().tr("Open the task output file"));
        setGraphic(FontIcon.of(UniconsLine.FILE_ALT));
    }

    @Override
    public void dispatch(DirectoryTaskOutput output) {
        destination = output.getDestination();
        setAccessibleText(i18n().tr("Open the task output directory"));
        setGraphic(FontIcon.of(UniconsLine.FOLDER_OPEN));
    }

    @Override
    public void dispatch(FileOrDirectoryTaskOutput output) {
        destination = output.getDestination();
        setAccessibleText(i18n().tr("Open the task output directory"));
        setGraphic(FontIcon.of(UniconsLine.FOLDER_OPEN));
    }

    private class OpenWithMenuItem extends MenuItem {

        private OpenWithMenuItem(Tool tool) {
            setText(tool.descriptor().name());
            setOnAction((e) -> {
                eventStudio().broadcast(new ClearToolRequest(tool.id(), false, false), tool.id());
                eventStudio().broadcast(new SetActiveContentItemRequest(tool.id()));
                PdfLoadRequest loadEvent = new PdfLoadRequest(tool.id());
                latestOutput.stream().map(PdfDocumentDescriptor::newDescriptorNoPassword).forEach(loadEvent::add);
                eventStudio().broadcast(loadEvent, tool.id());
            });
        }
    }
}
