/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/lug/2014
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
package org.pdfsam.ui.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.eventstudio.Listener;
import org.sejda.model.pdf.PdfVersion;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class PdfVersionConstrainedCheckBoxTest extends ApplicationTest {

    private static final String MODULE = "MODULE";
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule(MODULE);
    private PdfVersionConstrainedCheckBox victim;

    @Override
    public void start(Stage stage) {
        victim = new PdfVersionConstrainedCheckBox(PdfVersion.VERSION_1_4, MODULE);
        Scene scene = new Scene(new HBox(victim));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void addConstraintOnSelect() {
        BasePdfVersionEventListener<AddPdfVersionConstraintEvent> listener = new BasePdfVersionEventListener<>();
        eventStudio().add(AddPdfVersionConstraintEvent.class, listener, MODULE);
        clickOn(victim);
        assertTrue(listener.hit);
    }

    @Test
    public void removeConstraintOnDeselect() {
        BasePdfVersionEventListener<RemovePdfVersionConstraintEvent> listener = new BasePdfVersionEventListener<>();
        eventStudio().add(RemovePdfVersionConstraintEvent.class, listener, MODULE);
        doubleClickOn(victim);
        assertTrue(listener.hit);
    }

    private static class BasePdfVersionEventListener<T extends BasePdfVersionEvent> implements Listener<T> {
        private boolean hit = false;

        @Override
        public void onEvent(T event) {
            this.hit = true;
            assertEquals(PdfVersion.VERSION_1_4, event.getPdfVersion());
        }
    }
}
