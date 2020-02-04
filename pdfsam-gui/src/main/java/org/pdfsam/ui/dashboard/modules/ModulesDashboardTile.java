/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/mag/2014
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
package org.pdfsam.ui.dashboard.modules;

import static org.pdfsam.ui.commons.SetActiveModuleRequest.activeteModule;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.io.File;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

import org.pdfsam.module.Module;
import org.pdfsam.module.ModuleInputOutputType;
import org.pdfsam.pdf.BaseFilesDroppedEvent;
import org.pdfsam.pdf.MultipleFilesDroppedEvent;
import org.pdfsam.pdf.SingleFileDroppedEvent;
import org.pdfsam.ui.commons.UrlButton;

import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import de.jensd.fx.glyphs.materialdesignicons.utils.MaterialDesignIconFactory;
import javafx.scene.input.DragEvent;
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
            helpButton.setGraphic(MaterialDesignIconFactory.get().createIcon(MaterialDesignIcon.HELP_CIRCLE, "1.4em"));
            toolButtons.getChildren().add(helpButton);
            toolButtons.getStyleClass().add("dashboard-modules-toolbar");
            addBottomPanel(toolButtons);
        });
        setOnDragOver(e -> dragConsume(e, this.onDragOverConsumer()));
        if (module.descriptor().hasInputType(ModuleInputOutputType.MULTIPLE_PDF)) {
            setOnDragDropped(
                    e -> dragConsume(e, this.onDragDropped((files -> new MultipleFilesDroppedEvent(id, files)))));
        } else if (module.descriptor().hasInputType(ModuleInputOutputType.SINGLE_PDF)) {
            setOnDragDropped(e -> dragConsume(e, this.onDragDropped((files -> new SingleFileDroppedEvent(id, files)))));
        }

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

    private Consumer<DragEvent> onDragDropped(Function<List<File>, ? extends BaseFilesDroppedEvent> provider) {
        return (DragEvent e) -> {
            eventStudio().broadcast(activeteModule(id));
            eventStudio().broadcast(provider.apply(e.getDragboard().getFiles()));
            e.setDropCompleted(true);
        };
    }
}
