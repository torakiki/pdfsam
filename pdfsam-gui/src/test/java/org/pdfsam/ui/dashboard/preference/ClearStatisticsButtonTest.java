/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ago/2014
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
package org.pdfsam.ui.dashboard.preference;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.Parent;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.module.ClearUsageRequestEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.eventstudio.Listener;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class ClearStatisticsButtonTest extends GuiTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();

    @Override
    protected Parent getRootNode() {
        ClearStatisticsButton victim = new ClearStatisticsButton();
        victim.setId("victim");
        return victim;
    }

    @Test
    public void eventSent() {
        Listener<ClearUsageRequestEvent> listener = mock(Listener.class);
        eventStudio().add(ClearUsageRequestEvent.class, listener);
        click("#victim");
        verify(listener).onEvent(any());
    }

}
