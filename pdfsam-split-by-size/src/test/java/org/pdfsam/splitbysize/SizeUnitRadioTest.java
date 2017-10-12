/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12/dic/2014
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
package org.pdfsam.splitbysize;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.junit.Rule;
import org.junit.Test;
import org.pdfsam.test.InitializeAndApplyJavaFxThreadRule;

/**
 * @author Andrea Vacondio
 *
 */
public class SizeUnitRadioTest {
    @Rule
    public InitializeAndApplyJavaFxThreadRule fxThread = new InitializeAndApplyJavaFxThreadRule();

    @Test(expected = IllegalArgumentException.class)
    public void requiredSizeUnit() {
        new SizeUnitRadio(null);
    }

    @Test
    public void onSaveState() {
        SizeUnitRadio victim = new SizeUnitRadio(SizeUnit.KILOBYTE);
        victim.setSelected(true);
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertTrue(Boolean.valueOf(data.get(SizeUnit.KILOBYTE.toString())));
    }

    @Test
    public void onSaveStateNotSelected() {
        SizeUnitRadio victim = new SizeUnitRadio(SizeUnit.MEGABYTE);
        victim.setSelected(false);
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertFalse(Boolean.valueOf(data.get(SizeUnit.MEGABYTE.toString())));
    }

    @Test
    public void onRestoreState() {
        SizeUnitRadio victim = new SizeUnitRadio(SizeUnit.MEGABYTE);
        victim.setSelected(false);
        Map<String, String> data = new HashMap<>();
        data.put(SizeUnit.MEGABYTE.toString(), Boolean.TRUE.toString());
        victim.restoreStateFrom(data);
        assertTrue(victim.isSelected());
    }
}
