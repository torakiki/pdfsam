/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 19/apr/2014
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
package org.pdfsam.ui.log;

import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import javax.inject.Inject;

import org.pdfsam.context.UserContext;
import org.pdfsam.ui.support.CircularObservableList;
import org.sejda.eventstudio.Listener;

import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.SelectionMode;
import javafx.stage.Window;
import javafx.util.Callback;

/**
 * {@link ListView} showing log messages
 * 
 * @author Andrea Vacondio
 *
 */
class LogListView extends ListView<LogMessage> implements Listener<LogMessage> {

    @Inject
    public LogListView(UserContext userContext) {
        CircularObservableList<LogMessage> items = new CircularObservableList<>(userContext.getNumberOfLogRows());
        eventStudio().add(MaxLogRowsChangedEvent.class, e -> items.setMaxCapacity(userContext.getNumberOfLogRows()));
        setId("log-view");
        setItems(items);
        getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
        setCellFactory(new Callback<ListView<LogMessage>, ListCell<LogMessage>>() {
            @Override
            public ListCell<LogMessage> call(ListView<LogMessage> list) {
                return new TextCell();
            }
        });
    }

    static class TextCell extends ListCell<LogMessage> {
        @Override
        public void updateItem(LogMessage item, boolean empty) {
            super.updateItem(item, empty);
            for (LogLevel current : LogLevel.values()) {
                getStyleClass().remove(current.style());
            }
            if (nonNull(item)) {
                setText(item.getMessage());
                getStyleClass().add(item.getLevel().style());
            } else {
                setText("");
            }
        }
    }

    @Override
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
