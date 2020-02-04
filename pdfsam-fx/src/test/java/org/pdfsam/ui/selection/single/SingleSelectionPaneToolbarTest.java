/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 4 mag 2019
 * Copyright 2017 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.ui.selection.single;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.mockito.ArgumentCaptor;
import org.pdfsam.NoHeadless;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.commons.ClearModuleEvent;
import org.pdfsam.ui.selection.single.SingleSelectionPaneToolbar.ClearButton;
import org.pdfsam.eventstudio.Listener;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class SingleSelectionPaneToolbarTest extends ApplicationTest {

    private static final String MODULE = "MODULE";
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule(MODULE);
    private SingleSelectionPaneToolbar victim;

    @Override
    public void start(Stage stage) {
        victim = new SingleSelectionPaneToolbar(new Button("Roundkick!"), MODULE);
        victim.setId("victim");
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @Test
    @Category(NoHeadless.class)
    public void clear() {
        Listener<ClearModuleEvent> listener = mock(Listener.class);
        ArgumentCaptor<ClearModuleEvent> captor = ArgumentCaptor.forClass(ClearModuleEvent.class);
        eventStudio().add(ClearModuleEvent.class, listener, MODULE);
        clickOn(b -> b instanceof ClearButton);
        verify(listener).onEvent(captor.capture());
        assertTrue(captor.getValue().clearEverything);
    }

}
