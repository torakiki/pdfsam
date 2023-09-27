/*
 * This file is part of the PDF Split And Merge source code
 * Created on 4 mag 2019
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
package org.pdfsam.ui.components.selection.single;

import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.SplitMenuButton;
import javafx.stage.Stage;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.mockito.ArgumentCaptor;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.model.tool.ClearToolRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.HitTestListener;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ApplicationExtension.class)
public class SingleSelectionPaneToolbarTest {

    private static final String MODULE = "MODULE";
    @RegisterExtension
    static ClearEventStudioExtension staticExtension = new ClearEventStudioExtension(MODULE);
    private SingleSelectionPaneToolbar victim;

    @Start
    public void start(Stage stage) {
        victim = new SingleSelectionPaneToolbar(new Button("Roundkick!"), MODULE);
        victim.setId("victim");
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void clear(FxRobot robot) {
        HitTestListener<ClearToolRequest> listener = new HitTestListener<>();
        eventStudio().add(ClearToolRequest.class, listener);
        robot.clickOn(b -> b instanceof SingleSelectionPaneToolbar.ClearButton);
        assertTrue(listener.isHit());
    }

    @Test
    @Tag("NoHeadless")
    public void clearAllSettings(FxRobot robot) {
        Listener<ClearToolRequest> listener = mock(Listener.class);
        ArgumentCaptor<ClearToolRequest> captor = ArgumentCaptor.forClass(ClearToolRequest.class);
        eventStudio().add(ClearToolRequest.class, listener);
        SplitMenuButton btn = robot.lookup("#clear-button").queryAs(SplitMenuButton.class);
        for (Node child : btn.getChildrenUnmodifiable()) {
            if (child.getStyleClass().contains("arrow-button")) {
                robot.clickOn(child).clickOn(".menu-item");
            }
        }
        verify(listener).onEvent(captor.capture());
        assertTrue(captor.getValue().clearEverything());
    }
}
