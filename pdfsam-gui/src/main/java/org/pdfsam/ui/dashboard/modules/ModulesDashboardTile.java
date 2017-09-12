/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/mag/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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
package org.pdfsam.ui.dashboard.modules;

import static java.util.Arrays.stream;
import static org.pdfsam.ui.commons.SetActiveModuleRequest.activeteModule;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Stream;

import org.pdfsam.module.Module;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.support.io.FileType;
import org.pdfsam.ui.commons.UrlButton;

import de.jensd.fx.glyphs.GlyphsDude;
import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import javafx.scene.input.DragEvent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.TransferMode;
import javafx.scene.layout.VBox;

/**
 * Tile for the dashboard modules pane. It displays details about the {@link Module} and by clicking it the module will be opened in the Workarea.
 * 
 * @author Andrea Vacondio
 *
 */
class ModulesDashboardTile extends DashboardTile {

    private VBox toolButtons = new VBox(5);
    private String id;

    ModulesDashboardTile(Module module) {
        super(module.descriptor().getName(), module.descriptor().getDescription(), module.graphic());
        this.id = module.id();
        setOnAction(e -> eventStudio().broadcast(activeteModule(id)));

        module.descriptor().getSupportURL().ifPresent(url -> {
            UrlButton helpButton = UrlButton.urlButton(null, url, null, "pdfsam-toolbar-button");
            helpButton.setGraphic(GlyphsDude.createIcon(MaterialDesignIcon.HELP_CIRCLE, "1.4em"));
            toolButtons.getChildren().add(helpButton);
            toolButtons.getStyleClass().add("dashboard-modules-toolbar");
            addBottomPanel(toolButtons);
        });
        setOnDragOver(e -> dragConsume(e, this.onDragOverConsumer()));
        setOnDragDropped(e -> dragConsume(e, this.onDragDropped()));
    }

    private void dragConsume(DragEvent e, Consumer<DragEvent> c) {
        if (e.getDragboard().hasFiles()) {
            c.accept(e);
        }
        e.consume();
    }

    private Consumer<DragEvent> onDragOverConsumer() {
        return (DragEvent e) -> e.acceptTransferModes(TransferMode.COPY_OR_MOVE);
    }

    private Consumer<DragEvent> onDragDropped() {
        return (DragEvent e) -> {
            final PdfLoadRequestEvent loadEvent = new PdfLoadRequestEvent(id);
            getFilesFromDragboard(e.getDragboard()).filter(f -> FileType.PDF.matches(f.getName()))
                    .map(PdfDocumentDescriptor::newDescriptorNoPassword).forEach(loadEvent::add);
            if (!loadEvent.getDocuments().isEmpty()) {
                eventStudio().broadcast(activeteModule(id));
                eventStudio().broadcast(loadEvent, id);
            }
            e.setDropCompleted(true);
        };
    }

    private Stream<File> getFilesFromDragboard(Dragboard board) {
        List<File> files = board.getFiles();
        if (files.size() == 1 && files.get(0).isDirectory()) {
            return stream(files.get(0).listFiles()).sorted();
        }
        return files.stream();
    }
}
