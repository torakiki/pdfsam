/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09/ago/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.ui.components.selection.multiple;

import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.SplitMenuButton;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.mockito.ArgumentCaptor;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.model.tool.ClearToolRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.ui.components.selection.RemoveSelectedEvent;
import org.pdfsam.ui.components.selection.multiple.SelectionTableToolbar.ClearButton;
import org.pdfsam.ui.components.selection.multiple.SelectionTableToolbar.MoveDownButton;
import org.pdfsam.ui.components.selection.multiple.SelectionTableToolbar.MoveUpButton;
import org.pdfsam.ui.components.selection.multiple.SelectionTableToolbar.RemoveButton;
import org.pdfsam.ui.components.selection.multiple.move.MoveSelectedEvent;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.util.WaitForAsyncUtils;

import static java.util.Arrays.asList;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.ui.components.selection.multiple.SelectionChangedEvent.select;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(ApplicationExtension.class)
public class SelectionTableToolbarTest {

    private static final String MODULE = "MODULE";
    @RegisterExtension
    static ClearEventStudioExtension staticExtension = new ClearEventStudioExtension(MODULE);
    private SelectionTableToolbar victim;

    @Start
    public void start(Stage stage) {
        victim = new SelectionTableToolbar(MODULE, true);
        victim.setId("victim");
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void clear(FxRobot robot) {
        HitTestListener<ClearToolRequest> listener = new HitTestListener<>();
        eventStudio().add(ClearToolRequest.class, listener);
        robot.clickOn(b -> b instanceof ClearButton);
        assertTrue(listener.isHit());
    }

    @Test
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

    @Test
    public void add() {
        // TODO don't know how to test file choosers
    }

    @Test
    public void remove(FxRobot robot) {
        HitTestListener<RemoveSelectedEvent> listener = new HitTestListener<>();
        eventStudio().add(RemoveSelectedEvent.class, listener, MODULE);
        Node victim = robot.lookup(b -> b instanceof RemoveButton).query();
        enableByFiringSelectionChange(victim);
        robot.clickOn(victim);
        assertTrue(listener.isHit());
    }

    @Test
    public void moveUp(FxRobot robot) {
        HitTestListener<MoveSelectedEvent> listener = new HitTestListener<>();
        eventStudio().add(MoveSelectedEvent.class, listener, MODULE);
        Node victim = robot.lookup(b -> b instanceof MoveUpButton).query();
        enableByFiringSelectionChange(victim);
        robot.clickOn(victim);
        assertTrue(listener.isHit());
    }

    @Test
    public void moveDown(FxRobot robot) {
        HitTestListener<MoveSelectedEvent> listener = new HitTestListener<>();
        eventStudio().add(MoveSelectedEvent.class, listener, MODULE);
        Node victim = robot.lookup(b -> b instanceof MoveDownButton).query();
        enableByFiringSelectionChange(victim);
        robot.clickOn(victim);
        assertTrue(listener.isHit());
    }

    private void enableByFiringSelectionChange(Node victim) {
        assertTrue(victim.isDisabled());
        WaitForAsyncUtils.waitForAsyncFx(2000,
                () -> eventStudio().broadcast(select(asList(2, 3)).ofTotalRows(5), MODULE));
        assertFalse(victim.isDisabled());
    }
}
