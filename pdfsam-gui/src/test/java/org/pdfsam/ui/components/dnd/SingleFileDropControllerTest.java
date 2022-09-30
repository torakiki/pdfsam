/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 31 ago 2019
 * Copyright 2019 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.ui.components.dnd;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import java.io.IOException;
import java.util.Arrays;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.pdf.SingleFileDroppedEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.HitTestListener;

/**
 * @author Andrea Vacondio
 *
 */
public class SingleFileDropControllerTest {

    @Rule
    public ClearEventStudioRule cleanStudio = new ClearEventStudioRule();
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private HitTestListener<PdfLoadRequestEvent> listener;
    private static final String MODULE = "module";
    private SingleFileDropController victim;

    @Before
    public void setUp() {
        victim = new SingleFileDropController();
        listener = new HitTestListener<>();
        eventStudio().add(PdfLoadRequestEvent.class, listener, MODULE);
    }

    @Test
    public void noPdf() throws IOException {
        victim.request(new SingleFileDroppedEvent(MODULE, Arrays.asList(folder.newFile())));
        assertFalse(listener.isHit());
    }

    @Test
    public void directory() throws IOException {
        victim.request(new SingleFileDroppedEvent(MODULE, Arrays.asList(folder.newFolder())));
        assertFalse(listener.isHit());
    }

    @Test
    public void onePdf() throws IOException {
        victim.request(new SingleFileDroppedEvent(MODULE, Arrays.asList(folder.newFile(), folder.newFile("test.pdf"))));
        assertTrue(listener.isHit());
    }
}
