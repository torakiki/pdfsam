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
package org.pdfsam.ui.log;

import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.ClassRule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.pdfsam.NoHeadless;
import org.pdfsam.context.UserContext;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Provides;
import org.pdfsam.test.ClearEventStudioRule;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Scene;
import javafx.scene.input.Clipboard;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class LogPaneTest extends ApplicationTest {

    @ClassRule
    public static ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    private Injector injector;

    static class Config {
        @Provides
        UserContext context() {
            UserContext userContext = mock(UserContext.class);
            when(userContext.getNumberOfLogRows()).thenReturn(200);
            return userContext;
        }

        @Provides
        public LogPane pane(LogListView view) {
            return new LogPane(view);
        }

        @Provides
        public LogListView view() {
            LogListView view = new LogListView(context());
            view.onEvent(new LogMessage("A message", LogLevel.INFO));
            view.onEvent(new LogMessage("An Error message", LogLevel.ERROR));
            return view;
        }
    }

    @Override
    public void start(Stage stage) {
        injector = Injector.start(new Config());
        Scene scene = new Scene(injector.instance(LogPane.class));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    @Category(NoHeadless.class)
    public void clear() {
        LogListView view = injector.instance(LogListView.class);
        assertEquals(2, view.getItems().size());
        rightClickOn("A message").clickOn("#clearLogMenuItem");
        assertEquals(0, view.getItems().size());
    }

    @Test
    @Category(NoHeadless.class)
    public void copy() {
        WaitForAsyncUtils.waitForAsyncFx(2000, () -> {
            Clipboard.getSystemClipboard().clear();
            assertTrue(isBlank(Clipboard.getSystemClipboard().getString()));
        });
        clickOn("A message").rightClickOn("A message").clickOn("#copyLogMenuItem");
        WaitForAsyncUtils.waitForAsyncFx(2000,
                () -> assertTrue(Clipboard.getSystemClipboard().getString().contains("A message")));
    }

    @Test
    @Category(NoHeadless.class)
    public void selectAll() {
        LogListView view = injector.instance(LogListView.class);
        rightClickOn("A message").clickOn("#selectAllLogMenuItem");
        assertEquals(2, view.getSelectionModel().getSelectedItems().size());
    }

}
