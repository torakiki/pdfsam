/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/mar/2014
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
package org.pdfsam.ui.module;

import static java.util.Objects.isNull;
import static java.util.Objects.requireNonNull;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.ui.commons.SetActiveModuleRequest.activeteModule;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.Module;
import org.pdfsam.module.ModuleInputOutputType;
import org.pdfsam.module.TaskExecutionRequestEvent;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.ui.commons.ClearModuleEvent;
import org.pdfsam.ui.commons.OpenFileRequest;
import org.pdfsam.ui.support.Style;
import org.sejda.eventstudio.ReferenceStrength;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.eventstudio.annotation.EventStation;
import org.sejda.model.exception.TaskOutputVisitException;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.output.DirectoryTaskOutput;
import org.sejda.model.output.FileOrDirectoryTaskOutput;
import org.sejda.model.output.FileTaskOutput;
import org.sejda.model.output.TaskOutputDispatcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import de.jensd.fx.glyphs.fontawesome.utils.FontAwesomeIconFactory;
import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import de.jensd.fx.glyphs.materialdesignicons.utils.MaterialDesignIconFactory;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SplitMenuButton;

/**
 * Button to open the latest manipulation result
 * 
 * @author Andrea Vacondio
 *
 */
public class OpenButton extends SplitMenuButton implements TaskOutputDispatcher {

    private static final Logger LOG = LoggerFactory.getLogger(OpenButton.class);
    private String ownerModule = StringUtils.EMPTY;
    private File destination;
    private List<File> latestOutput = new ArrayList<>();
    private ModuleInputOutputType outputType;

    public OpenButton(String ownerModule, ModuleInputOutputType outputType) {
        requireNonNull(outputType);
        this.outputType = outputType;
        this.ownerModule = defaultString(ownerModule);
        setId(ownerModule + ".openButton");
        getStyleClass().addAll(Style.BUTTON.css());
        getStyleClass().addAll("pdfsam-split-button", "footer-open-button");
        setText(DefaultI18nContext.getInstance().i18n("Open"));
        setMaxHeight(Double.MAX_VALUE);
        setPrefHeight(Double.MAX_VALUE);
        setVisible(false);
        setOnAction(e -> {
            if (latestOutput.size() != 1 || !openFile(latestOutput.get(0))) {
                openFile(destination);
            }

        });
        eventStudio().add(TaskExecutionRequestEvent.class, e -> {
            if (e.getModuleId().equals(ownerModule)) {
                latestOutput.clear();
                try {
                    if (!isNull(e.getParameters().getOutput())) {
                        e.getParameters().getOutput().accept(this);
                    }
                } catch (TaskOutputVisitException ex) {
                    LOG.warn("This should never happen", ex);
                }
            }
        }, -10, ReferenceStrength.STRONG);
        eventStudio().addAnnotatedListeners(this);
    }

    private boolean openFile(File file) {
        if (file != null && file.exists()) {
            eventStudio().broadcast(new OpenFileRequest(file));
            return true;
        }
        return false;
    }

    public void initModules(Collection<Module> modules) {
        modules.forEach(m -> {
            if (m.descriptor().hasInputType(outputType)) {
                getItems().add(new OpenWithMenuItem(m));
            }
        });
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
        setGraphic(FontAwesomeIconFactory.get().createIcon(FontAwesomeIcon.FILE_PDF_ALT, "1.6em"));
    }

    @Override
    public void dispatch(DirectoryTaskOutput output) {
        destination = output.getDestination();
        setGraphic(MaterialDesignIconFactory.get().createIcon(MaterialDesignIcon.FOLDER_OUTLINE, "1.6em"));
    }

    @Override
    public void dispatch(FileOrDirectoryTaskOutput output) {
        destination = output.getDestination();
        setGraphic(MaterialDesignIconFactory.get().createIcon(MaterialDesignIcon.FOLDER_OUTLINE, "1.6em"));
    }

    private class OpenWithMenuItem extends MenuItem {

        private OpenWithMenuItem(Module module) {
            setText(module.descriptor().getName());
            setOnAction((e) -> {
                eventStudio().broadcast(new ClearModuleEvent(), module.id());
                eventStudio().broadcast(activeteModule(module.id()));
                PdfLoadRequestEvent loadEvent = new PdfLoadRequestEvent(module.id());
                latestOutput.stream().map(PdfDocumentDescriptor::newDescriptorNoPassword).forEach(loadEvent::add);
                eventStudio().broadcast(loadEvent, module.id());
            });
        }
    }
}
