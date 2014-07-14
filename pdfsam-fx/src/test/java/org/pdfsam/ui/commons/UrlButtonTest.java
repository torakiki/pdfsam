/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/lug/2014
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
package org.pdfsam.ui.commons;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.Parent;

import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.sejda.eventstudio.Listener;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class UrlButtonTest extends GuiTest {

    private static final String URL = "http://www.example.com";

    @Test(expected = IllegalArgumentException.class)
    public void nullUrl() {
        new UrlButton("Chuck", null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void emptyUrl() {
        new UrlButton("Chuck", "");
    }

    @Test
    public void eventSent() {
        OpenUrlRequestListener listener = new OpenUrlRequestListener();
        eventStudio().add(listener);
        try {
            click(".pdfsam-button");
            assertTrue(listener.hit);
        } finally {
            eventStudio().remove(listener);
        }
    }

    @Override
    protected Parent getRootNode() {
        return new UrlButton("Chuck", URL);
    }

    private static class OpenUrlRequestListener implements Listener<OpenUrlRequest> {

        private boolean hit = false;

        public void onEvent(OpenUrlRequest event) {
            assertEquals(URL, event.getUrl());
            this.hit = true;
        }

    }

}
