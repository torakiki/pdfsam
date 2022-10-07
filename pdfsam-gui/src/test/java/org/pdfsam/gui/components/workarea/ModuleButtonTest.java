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
package org.pdfsam.gui.components.workarea;

import javafx.scene.Scene;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.pdfsam.eventstudio.Listener;
import org.pdfsam.model.ui.SetActiveToolRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.DefaultPriorityTestTool;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 *
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
public class ModuleButtonTest   {

    private DefaultPriorityTestTool module = new DefaultPriorityTestTool();
    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        Scene scene = new Scene(new ToolButton(module));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void onClick() {
        Listener<SetActiveToolRequest> listener = mock(Listener.class);
        eventStudio().add(SetActiveToolRequest.class, listener);
        robot.clickOn(module.descriptor().name());
        ArgumentCaptor<SetActiveToolRequest> captor = ArgumentCaptor.forClass(SetActiveToolRequest.class);
        verify(listener).onEvent(captor.capture());
        assertEquals(module.id(), captor.getValue().id());
    }

    @Test
    public void requiredModule() {
        assertThrows(IllegalArgumentException.class, () -> new ToolButton(null));
    }

}
