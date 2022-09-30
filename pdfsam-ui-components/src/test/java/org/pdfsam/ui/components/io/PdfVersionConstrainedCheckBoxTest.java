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
package org.pdfsam.ui.components.io;

import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.pdfsam.model.ui.AddPdfVersionConstraintEvent;
import org.pdfsam.model.ui.RemovePdfVersionConstraintEvent;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.HitTestListener;
import org.sejda.model.pdf.PdfVersion;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class })
public class PdfVersionConstrainedCheckBoxTest {

    private static final String MODULE = "MODULE";
    @RegisterExtension
    public ClearEventStudioExtension clearStudio = new ClearEventStudioExtension(MODULE);
    private PdfVersionConstrainedCheckBox victim;

    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        victim = new PdfVersionConstrainedCheckBox(PdfVersion.VERSION_1_4, MODULE);
        Scene scene = new Scene(new HBox(victim));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void addConstraintOnSelect() {
        HitTestListener<AddPdfVersionConstraintEvent> listener = new HitTestListener<>() {
            @Override
            public void onEvent(AddPdfVersionConstraintEvent event) {
                super.onEvent(event);
                assertEquals(PdfVersion.VERSION_1_4, event.pdfVersion());
            }
        };
        eventStudio().add(AddPdfVersionConstraintEvent.class, listener, MODULE);
        robot.clickOn(victim);
        assertTrue(listener.isHit());
    }

    @Test
    public void removeConstraintOnDeselect() {
        HitTestListener<RemovePdfVersionConstraintEvent> listener = new HitTestListener<>() {
            @Override
            public void onEvent(RemovePdfVersionConstraintEvent event) {
                super.onEvent(event);
                assertEquals(PdfVersion.VERSION_1_4, event.pdfVersion());
            }
        };
        eventStudio().add(RemovePdfVersionConstraintEvent.class, listener, MODULE);
        robot.doubleClickOn(victim);
        assertTrue(listener.isHit());
    }

}
