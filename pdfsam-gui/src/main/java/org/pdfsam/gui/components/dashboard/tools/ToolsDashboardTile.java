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
package org.pdfsam.gui.components.dashboard.tools;

import javafx.scene.input.DragEvent;
import javafx.scene.input.TransferMode;
import javafx.scene.layout.VBox;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.tool.ToolInputOutputType;
import org.pdfsam.model.ui.SetActiveToolRequest;
import org.pdfsam.model.ui.dnd.FilesDroppedEvent;
import org.pdfsam.ui.components.commons.UrlButton;

import java.io.File;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

import static java.util.Optional.ofNullable;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * Tile for the dashboard tools pane. It displays details about the {@link Tool} and by clicking it the tool will be opened in the Workarea.
 *
 * @author Andrea Vacondio
 */
class ToolsDashboardTile extends DashboardTile {

    private final VBox toolButtons = new VBox(5);
    private final String id;

    ToolsDashboardTile(Tool tool) {
        super(tool.descriptor().name(), tool.descriptor().description(), tool.graphic());
        this.id = tool.id();
        setOnAction(e -> eventStudio().broadcast(new SetActiveToolRequest(id)));

        ofNullable(tool.descriptor().supportUrl()).ifPresent(url -> {
            UrlButton helpButton = UrlButton.urlButton(null, url, null, "pdfsam-toolbar-button");
            helpButton.setGraphic(FontIcon.of(UniconsLine.QUESTION_CIRCLE, 18));
            toolButtons.getChildren().add(helpButton);
            toolButtons.getStyleClass().add("dashboard-modules-toolbar");
            addBottomPanel(toolButtons);
        });
        setOnDragOver(e -> dragConsume(e, this.onDragOverConsumer()));
        setOnDragDropped(e -> dragConsume(e, this.onDragDropped(
                files -> new FilesDroppedEvent(id, tool.descriptor().hasInputType(ToolInputOutputType.MULTIPLE_PDF),
                        files))));

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

    private Consumer<DragEvent> onDragDropped(Function<List<File>, ?> provider) {
        return (DragEvent e) -> {
            eventStudio().broadcast(new SetActiveToolRequest(id));
            eventStudio().broadcast(provider.apply(e.getDragboard().getFiles()));
            e.setDropCompleted(true);
        };
    }
}
