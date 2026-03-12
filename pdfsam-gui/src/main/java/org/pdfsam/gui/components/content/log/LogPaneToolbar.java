/*
 * This file is part of the PDF Split And Merge source code
 * Created on 15/01/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.gui.components.content.log;

import jakarta.inject.Inject;
import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.scene.control.Button;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import org.pdfsam.model.log.ClearLogRequest;
import org.pdfsam.model.log.SaveLogRequest;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.ui.SetActiveContentItemRequest;
import org.pdfsam.ui.components.support.Style;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Toolbar for the log pane
 *
 * @author Andrea Vacondio
 */
class LogPaneToolbar extends ToolBar {

    @Inject
    public LogPaneToolbar(LogListView logView) {
        var clearItem = new ClearButton();
        clearItem.disableProperty().bind(Bindings.isEmpty(logView.getItems()));
        var saveItem = new SaveButton();
        saveItem.disableProperty().bind(clearItem.disableProperty());
        var closeItem = new CloseButton();
        getItems().addAll(saveItem, clearItem, closeItem);
        getStyleClass().add("log-tool-bar");
    }

    static class ClearButton extends Button {
        public ClearButton() {
            setTooltip(new Tooltip(i18n().tr("Removes all the log messages")));
            setText(i18n().tr("_Clear"));
            setOnAction(e -> eventStudio().broadcast(new ClearLogRequest()));
            getStyleClass().addAll(Style.BUTTON.css());
            getStyleClass().addAll(Style.TOOLBAR_BUTTON.css());
        }
    }

    static class SaveButton extends Button {
        public SaveButton() {
            setText(i18n().tr("_Save"));
            setTooltip(new Tooltip(i18n().tr("Save the log messages to a text file")));
            setOnAction(e -> eventStudio().broadcast(new SaveLogRequest()));
            getStyleClass().addAll(Style.BUTTON.css());
            getStyleClass().addAll(Style.TOOLBAR_BUTTON.css());
        }
    }

    class CloseButton extends Button {
        public CloseButton() {
            setText(i18n().tr("C_lose"));
            setTooltip(new Tooltip(i18n().tr("Close the log panel")));
            app().runtimeState().activeTool().subscribe(t -> {
                Platform.runLater(() -> {
                    this.setVisible(t.isPresent());
                    t.map(Tool::id).map(SetActiveContentItemRequest::new)
                            .ifPresentOrElse(a -> setOnAction(e -> eventStudio().broadcast(a)),
                                    () -> setOnAction(null));
                });
            });
            getStyleClass().addAll(Style.BUTTON.css());
            getStyleClass().addAll(Style.TOOLBAR_BUTTON.css());
        }
    }
}
