/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/ago/2014
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
package org.pdfsam.ui.workarea;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import org.junit.ClassRule;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.pdfsam.ui.commons.SetActiveModuleRequest;
import org.pdfsam.eventstudio.Listener;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class ModuleButtonTest extends ApplicationTest {

    @ClassRule
    public static ClearEventStudioRule STUDIO = new ClearEventStudioRule();
    private DefaultPriorityTestTool module = new DefaultPriorityTestTool();

    @Override
    public void start(Stage stage) {
        Scene scene = new Scene(new ModuleButton(module));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void onClick() {
        Listener<SetActiveModuleRequest> listener = mock(Listener.class);
        eventStudio().add(SetActiveModuleRequest.class, listener);
        clickOn(module.descriptor().getName());
        ArgumentCaptor<SetActiveModuleRequest> captor = ArgumentCaptor.forClass(SetActiveModuleRequest.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(module.id(), captor.getValue().getActiveModuleId().get());
    }

    @Test(expected = IllegalArgumentException.class)
    public void requiredModule() {
        new ModuleButton(null);
    }

}
