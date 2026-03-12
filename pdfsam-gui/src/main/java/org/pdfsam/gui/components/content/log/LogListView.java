/*
 * This file is part of the PDF Split And Merge source code
 * Created on 19/apr/2014
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
package org.pdfsam.gui.components.content.log;

import jakarta.inject.Inject;
import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.SelectionMode;
import javafx.stage.Window;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.ui.components.support.CircularObservableList;

import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.core.context.IntegerPersistentProperty.LOGVIEW_ROWS_NUMBER;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * {@link ListView} showing log messages
 *
 * @author Andrea Vacondio
 */
class LogListView extends ListView<LogMessage> {

    @Inject
    public LogListView() {
        CircularObservableList<LogMessage> items = new CircularObservableList<>(
                app().persistentSettings().get(LOGVIEW_ROWS_NUMBER));
        eventStudio().add(MaxLogRowsChangedEvent.class,
                e -> items.setMaxCapacity(app().persistentSettings().get(LOGVIEW_ROWS_NUMBER)));
        setId("log-view");
        setAccessibleText(i18n().tr("Log messages"));
        setItems(items);
        getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
        setCellFactory(list -> new TextCell());
        eventStudio().addAnnotatedListeners(this);
    }

    static class TextCell extends ListCell<LogMessage> {
        @Override
        public void updateItem(LogMessage item, boolean empty) {
            super.updateItem(item, empty);
            for (LogLevel current : LogLevel.values()) {
                getStyleClass().remove(current.style());
            }
            if (nonNull(item)) {
                setText(item.message());
                setAccessibleText(item.level().name() + ": " + item.message());
                getStyleClass().add(item.level().style());
            } else {
                setText("");
                setAccessibleText(null);
            }
        }
    }

    @EventListener
    public void onEvent(LogMessage event) {
        Platform.runLater(() -> {
            getItems().add(event);
            scrollToBottomIfShowing();
        });
    }

    public void scrollToBottomIfShowing() {
        if (!getItems().isEmpty()
                && ofNullable(this.getScene()).map(Scene::getWindow).map(Window::isShowing).orElse(Boolean.TRUE)) {
            scrollTo(getItems().size() - 1);
        }
    }
}
