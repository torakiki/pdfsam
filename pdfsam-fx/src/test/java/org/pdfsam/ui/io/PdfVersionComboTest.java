/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 17/lug/2014
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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;
import org.pdfsam.ui.io.PdfVersionCombo.PdfVersionComboItem;
import org.sejda.model.pdf.PdfVersion;

/**
 * @author Andrea Vacondio
 *
 */
public class PdfVersionComboTest {

    private static final String MODULE = "MODULE";
    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule(MODULE);
    @Rule
    public InitializeAndApplyJavaFxThreadRule fxThread = new InitializeAndApplyJavaFxThreadRule();
    private PdfVersionCombo victim;

    @Before
    public void setUp() {
        victim = new PdfVersionCombo(MODULE);
    }

    @Test
    public void addConstraintRemovesItems() {
        eventStudio().broadcast(new AddPdfVersionConstraintEvent(PdfVersion.VERSION_1_5), MODULE);
        assertTrue(comboHasItem(victim, PdfVersion.VERSION_1_5));
        assertFalse(comboHasItem(victim, PdfVersion.VERSION_1_4));
    }

    @Test
    public void removeConstraintRestoresItems() {
        eventStudio().broadcast(new AddPdfVersionConstraintEvent(PdfVersion.VERSION_1_4), MODULE);
        eventStudio().broadcast(new AddPdfVersionConstraintEvent(PdfVersion.VERSION_1_6), MODULE);
        assertFalse(comboHasItem(victim, PdfVersion.VERSION_1_5));
        eventStudio().broadcast(new RemovePdfVersionConstraintEvent(PdfVersion.VERSION_1_6), MODULE);
        assertTrue(comboHasItem(victim, PdfVersion.VERSION_1_5));
    }

    @Test
    public void enableSameAsSource() {
        int comboSize = victim.getItems().size();
        victim.enableSameAsSourceItem();
        assertEquals(comboSize + 1, victim.getItems().size());
    }

    @Test
    public void sameAsSourceVersion() {
        victim.enableSameAsSourceItem();
        eventStudio().broadcast(new ChangedSelectedPdfVersionEvent(PdfVersion.VERSION_1_4), MODULE);
        long values = victim.getItems().stream().filter(i -> i.getVersion().equals(PdfVersion.VERSION_1_4)).count();
        assertEquals(2, values);
    }

    @Test
    public void sameAsSourceVersionLowerThenConstraint() {
        victim.enableSameAsSourceItem();
        eventStudio().broadcast(new AddPdfVersionConstraintEvent(PdfVersion.VERSION_1_5), MODULE);
        assertEquals(4, victim.getItems().size());
        eventStudio().broadcast(new ChangedSelectedPdfVersionEvent(PdfVersion.VERSION_1_4), MODULE);
        assertEquals(3, victim.getItems().size());
    }

    @Test
    public void lowestIsSelected() {
        victim.enableSameAsSourceItem();
        eventStudio().broadcast(new ChangedSelectedPdfVersionEvent(PdfVersion.VERSION_1_4), MODULE);
        eventStudio().broadcast(new AddPdfVersionConstraintEvent(PdfVersion.VERSION_1_5), MODULE);
        assertEquals(PdfVersion.VERSION_1_5, victim.getSelectionModel().getSelectedItem().getVersion());
    }

    @Test
    public void reset() {
        eventStudio().broadcast(new AddPdfVersionConstraintEvent(PdfVersion.VERSION_1_5), MODULE);
        assertEquals(PdfVersion.VERSION_1_5, victim.getSelectionModel().getSelectedItem().getVersion());
        assertEquals(3, victim.getItems().size());
        victim.resetView();
        assertEquals(5, victim.getItems().size());

    }

    private boolean comboHasItem(PdfVersionCombo combo, PdfVersion version) {
        for (PdfVersionComboItem item : combo.getItems()) {
            if (item.getVersion().equals(version)) {
                return true;
            }
        }
        return false;
    }
}
