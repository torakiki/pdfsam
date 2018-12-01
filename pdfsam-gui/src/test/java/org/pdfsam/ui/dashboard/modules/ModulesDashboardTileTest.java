/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/ago/2014
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
package org.pdfsam.ui.dashboard.modules;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.junit.Rule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.DefaultPriorityTestModule;
import org.pdfsam.ui.commons.OpenUrlRequest;
import org.pdfsam.ui.commons.SetActiveModuleRequest;
import org.sejda.eventstudio.Listener;
import org.testfx.api.FxAssert;
import org.testfx.framework.junit.ApplicationTest;

import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;
import javafx.scene.Scene;
import javafx.scene.input.MouseButton;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class ModulesDashboardTileTest extends ApplicationTest {

    @Rule
    public ClearEventStudioRule clearRule = new ClearEventStudioRule();

    @Override
    public void start(Stage stage) {
        Scene scene = new Scene(new ModulesDashboardTile(new DefaultPriorityTestModule()));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void activateOnClick() {
        Listener<SetActiveModuleRequest> listener = mock(Listener.class);
        eventStudio().add(SetActiveModuleRequest.class, listener);
        moveTo(".dashboard-modules-invisible-button").press(MouseButton.PRIMARY);
        FxAssert.verifyThat(".dashboard-modules-tile", (ModulesDashboardTile v) -> v.isArmed());
        release(MouseButton.PRIMARY);
        ArgumentCaptor<SetActiveModuleRequest> captor = ArgumentCaptor.forClass(SetActiveModuleRequest.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(DefaultPriorityTestModule.ID, captor.getValue().getActiveModuleId().get());
    }

    @Test
    public void supportVideoClick() {
        Listener<OpenUrlRequest> openUrlListener = mock(Listener.class);
        eventStudio().add(OpenUrlRequest.class, openUrlListener);
        ArgumentCaptor<OpenUrlRequest> openUrlCaptor = ArgumentCaptor.forClass(OpenUrlRequest.class);
        clickOn(MaterialDesignIcon.HELP_CIRCLE.unicode());
        verify(openUrlListener).onEvent(openUrlCaptor.capture());
        assertEquals("http://www.chucknorrisfacts.com/", openUrlCaptor.getValue().getUrl());
    }
}
