/*
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ago/2014
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
package org.pdfsam.gui.components.log;

import javafx.scene.Scene;
import javafx.scene.input.Clipboard;
import javafx.stage.Stage;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Provides;
import org.pdfsam.test.ClearEventStudioExtension;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.util.WaitForAsyncUtils;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
public class LogPaneTest {

    private Injector injector;
    private FxRobot robot;

    static class Config {

        @Provides
        public LogPane pane(LogListView view) {
            return new LogPane(view);
        }

        @Provides
        public LogListView view() {
            LogListView view = new LogListView();
            view.onEvent(new LogMessage("A message", LogLevel.INFO));
            view.onEvent(new LogMessage("An Error message", LogLevel.ERROR));
            return view;
        }
    }

    @Start
    public void start(Stage stage) {
        injector = Injector.start(new Config());
        Scene scene = new Scene(injector.instance(LogPane.class));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    @Tag("NoHeadless")
    public void clear() {
        LogListView view = injector.instance(LogListView.class);
        assertEquals(2, view.getItems().size());
        robot.rightClickOn("A message").clickOn("#clearLogMenuItem");
        assertEquals(0, view.getItems().size());
    }

    @Test
    @Tag("NoHeadless")
    public void copy() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            Clipboard.getSystemClipboard().clear();
            assertTrue(isBlank(Clipboard.getSystemClipboard().getString()));
        });
        robot.clickOn("A message").rightClickOn("A message").clickOn("#copyLogMenuItem");
        WaitForAsyncUtils.waitForAsyncFx(2000,
                () -> assertTrue(Clipboard.getSystemClipboard().getString().contains("A message")));
    }

    @Test
    @Tag("NoHeadless")
    public void selectAll() {
        LogListView view = injector.instance(LogListView.class);
        robot.rightClickOn("A message").clickOn("#selectAllLogMenuItem");
        assertEquals(2, view.getSelectionModel().getSelectedItems().size());
    }

}
