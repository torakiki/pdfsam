package org.pdfsam.gui.components.sidebar;

import javafx.scene.Scene;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.gui.components.content.home.HomePane;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.ui.ContentItem;
import org.pdfsam.model.ui.SetActiveContentItemRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.pdfsam.test.TestContentItem;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 20/01/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@pdfsam.org).
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
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
class SelectableSidebarButtonTest {
    private static final Tool TOOL = new DefaultPriorityTestTool();
    private static final ContentItem CONTENT_ITEM = new TestContentItem();

    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        mock(HomePane.class);
        Scene scene = new Scene(
                new VBox(5, SelectableSidebarButton.of(TOOL), SelectableSidebarButton.of(CONTENT_ITEM)));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void onClickTool() {
        Listener<SetActiveContentItemRequest> listener = mock(Listener.class);
        eventStudio().add(SetActiveContentItemRequest.class, listener);
        robot.clickOn(TOOL.descriptor().name());
        ArgumentCaptor<SetActiveContentItemRequest> captor = ArgumentCaptor.forClass(SetActiveContentItemRequest.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(TOOL.id(), captor.getValue().id());
    }

    @Test
    public void onClickContentItem() {
        Listener<SetActiveContentItemRequest> listener = mock(Listener.class);
        eventStudio().add(SetActiveContentItemRequest.class, listener);
        robot.clickOn(CONTENT_ITEM.name());
        ArgumentCaptor<SetActiveContentItemRequest> captor = ArgumentCaptor.forClass(SetActiveContentItemRequest.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(CONTENT_ITEM.id(), captor.getValue().id());
    }

    @Test
    @DisplayName("Selecting the button disables it")
    public void selectsDisables() {
        var victim = SelectableSidebarButton.of(CONTENT_ITEM);
        assertFalse(victim.isDisabled());
        victim.setSelected(true);
        assertTrue(victim.isDisabled());
    }

    @Test
    public void selectIf() {
        var victim = SelectableSidebarButton.of(CONTENT_ITEM);
        assertFalse(victim.isDisabled());
        victim.selectIf(CONTENT_ITEM.id());
        assertTrue(victim.isDisabled());
    }

    @Test
    public void selectIfWrongId() {
        var victim = SelectableSidebarButton.of(CONTENT_ITEM);
        assertFalse(victim.isDisabled());
        victim.selectIf("ImNotMatching");
        assertFalse(victim.isDisabled());
    }

    @Test
    void selectIfNull() {
        var victim = new SelectableSidebarButton("id", "label");
        victim.selectIf(null);
        assertFalse(victim.isSelected());
    }

    @Test
    void selectIfEmpty() {
        var victim = new SelectableSidebarButton("id", "label");
        victim.selectIf("");
        assertFalse(victim.isSelected());
    }

    @Test
    public void requiredTool() {
        assertThrows(IllegalArgumentException.class, () -> SelectableSidebarButton.of((Tool) null));
    }

    @Test
    public void requiredContent() {
        assertThrows(IllegalArgumentException.class, () -> SelectableSidebarButton.of((ContentItem) null));
    }

}