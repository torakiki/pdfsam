/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/ago/2014
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
package org.pdfsam.ui.banner;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.ui.commons.ShowStageRequest;
import org.sejda.eventstudio.Listener;

import javafx.scene.Parent;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class LogButtonTest extends GuiTest {
    @Override
    protected Parent getRootNode() {
        return new LogButton();
    }

    @Test
    public void onClick() {
        Listener<ShowStageRequest> listener = mock(Listener.class);
        eventStudio().add(ShowStageRequest.class, listener, "LogStage");
        click(".button");
        verify(listener).onEvent(any());
    }

    @Test
    public void setUpToDate() {
        LogButton victim = find(".button");
        assertFalse(victim.getStyleClass().contains(LogButton.HAS_ERRORS_CSS_CLASS));
        victim.hasUnseenErrors(true);
        assertTrue(victim.getStyleClass().contains(LogButton.HAS_ERRORS_CSS_CLASS));
        victim.hasUnseenErrors(false);
        assertFalse(victim.getStyleClass().contains(LogButton.HAS_ERRORS_CSS_CLASS));
    }
}
