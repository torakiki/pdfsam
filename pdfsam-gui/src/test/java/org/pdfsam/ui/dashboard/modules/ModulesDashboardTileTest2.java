/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 01/set/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Andrea Vacondio (info@pdfsam.org).
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

import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.mockito.ArgumentCaptor;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.DefaultPriorityTestModule;
import org.pdfsam.ui.commons.OpenUrlRequest;
import org.sejda.eventstudio.Listener;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import javafx.scene.Parent;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
@Ignore
public class ModulesDashboardTileTest2 extends GuiTest {
    @Rule
    public ClearEventStudioRule clearRule = new ClearEventStudioRule();

    @Override
    protected Parent getRootNode() {
        return new ModulesDashboardTile(new DefaultPriorityTestModule());
    }

    @Test
    public void supportVideoClick() {
        // TODO understand why if put in the same class it fails
        Listener<OpenUrlRequest> openUrlListener = mock(Listener.class);
        eventStudio().add(OpenUrlRequest.class, openUrlListener);
        ArgumentCaptor<OpenUrlRequest> openUrlCaptor = ArgumentCaptor.forClass(OpenUrlRequest.class);
        click(FontAwesomeIcon.YOUTUBE_PLAY.toString());
        verify(openUrlListener).onEvent(openUrlCaptor.capture());
        assertEquals("http://www.chucknorrisfacts.com/", openUrlCaptor.getValue().getUrl());
    }
}
