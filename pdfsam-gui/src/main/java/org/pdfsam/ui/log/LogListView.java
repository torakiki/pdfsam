/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 19/apr/2014
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
package org.pdfsam.ui.log;

import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.SelectionMode;
import javafx.util.Callback;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.UserContext;
import org.pdfsam.ui.support.CircularObservableList;

/**
 * {@link ListView} showing log messages
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class LogListView extends ListView<LogMessage> {

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

    void appendLog(LogLevel level, String message) {
        getItems().add(new LogMessage(message, level));
        scrollTo(getItems().size() - 1);
    }

    static class TextCell extends ListCell<LogMessage> {
        @Override
        public void updateItem(LogMessage item, boolean empty) {
            super.updateItem(item, empty);
            if (item != null) {
                Label msg = new Label(item.getMessage());
                msg.getStyleClass().add(item.getLevel().style());
                setGraphic(msg);
            } else {
                setGraphic(null);
            }
        }
    }

}
