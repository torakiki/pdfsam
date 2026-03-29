package org.pdfsam.ui.components.io;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 26/03/26
 * Copyright 2026 by Sober Lemur S.r.l. (info@soberlemur.com).
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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.pdfsam.model.ui.AddPdfVersionConstraintEvent;
import org.pdfsam.model.ui.ComboItem;
import org.pdfsam.model.ui.RemovePdfVersionConstraintEvent;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.HitTestListener;
import org.pdfsam.test.JavaFxThreadExtension;
import org.sejda.model.output.CompressionPolicy;
import org.sejda.model.pdf.PdfVersion;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

@ExtendWith(JavaFxThreadExtension.class)
public class PdfCompressionPolicyComboTest {

    private static final String MODULE = "MODULE";
    @RegisterExtension
    public ClearEventStudioExtension clearStudio = new ClearEventStudioExtension(MODULE);

    private PdfCompressionPolicyCombo victim;

    @BeforeEach
    public void setUp() {
        victim = new PdfCompressionPolicyCombo(MODULE);
    }

    @Test
    public void addConstraintOnSelect() {
        HitTestListener<AddPdfVersionConstraintEvent> listener = new HitTestListener<>() {
            @Override
            public void onEvent(AddPdfVersionConstraintEvent event) {
                super.onEvent(event);
                assertEquals(PdfVersion.VERSION_1_5, event.pdfVersion());
            }
        };
        eventStudio().add(AddPdfVersionConstraintEvent.class, listener, MODULE);
        victim.getSelectionModel().select(ComboItem.keyWithEmptyValue(CompressionPolicy.COMPRESS));
        assertTrue(listener.isHit());
    }

    @Test
    public void removeConstraintOnDeselect() {
        HitTestListener<RemovePdfVersionConstraintEvent> listener = new HitTestListener<>() {
            @Override
            public void onEvent(RemovePdfVersionConstraintEvent event) {
                super.onEvent(event);
                assertEquals(PdfVersion.VERSION_1_5, event.pdfVersion());
            }
        };
        eventStudio().add(RemovePdfVersionConstraintEvent.class, listener, MODULE);
        victim.getSelectionModel().select(ComboItem.keyWithEmptyValue(CompressionPolicy.NEUTRAL));
        assertTrue(listener.isHit());
    }
}