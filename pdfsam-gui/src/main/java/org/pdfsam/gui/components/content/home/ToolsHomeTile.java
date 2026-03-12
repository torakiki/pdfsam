/*
 * This file is part of the PDF Split And Merge source code
 * Created on 08/mag/2014
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
package org.pdfsam.gui.components.content.home;

import javafx.scene.AccessibleRole;
import javafx.scene.input.DragEvent;
import javafx.scene.input.TransferMode;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.tool.ToolInputOutputType;
import org.pdfsam.model.ui.SetActiveContentItemRequest;
import org.pdfsam.model.ui.dnd.FilesDroppedEvent;
import org.pdfsam.ui.components.commons.UrlButton;

import java.io.File;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

import static java.util.Optional.ofNullable;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Tile for the home panel that displays details about the {@link Tool}. Clicking the tile will send a request to open/show the tool panel
 *
 * @author Andrea Vacondio
 */
class ToolsHomeTile extends HomeTile {

    private final String id;

    ToolsHomeTile(Tool tool) {
        super(tool.descriptor().name(), tool.descriptor().description(), tool.graphic(),
                tool.descriptor().category().styleClass());
        this.id = tool.id();
        setOnAction(e -> eventStudio().broadcast(new SetActiveContentItemRequest(id)));

        ofNullable(tool.descriptor().supportUrl()).ifPresent(url -> {
            var helpIcon = FontIcon.of(UniconsLine.QUESTION_CIRCLE, 18);
            helpIcon.setAccessibleRole(AccessibleRole.IMAGE_VIEW);
            var helpButton = UrlButton.urlButton(null, url, null, "btn", "home-tools-help-button");
            helpButton.setGraphic(helpIcon);
            helpButton.setAccessibleText(i18n().tr("Help for {0}", tool.descriptor().name()));
            addBottomPanel(helpButton);
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
            eventStudio().broadcast(new SetActiveContentItemRequest(id));
            eventStudio().broadcast(provider.apply(e.getDragboard().getFiles()));
            e.setDropCompleted(true);
        };
    }
}
