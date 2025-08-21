/*
 * This file is part of the PDF Split And Merge source code
 * Created on 12/dic/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.tools.splitbysize;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.model.ui.workspace.WorkspaceData;
import org.pdfsam.test.JavaFxThreadInitializeExtension;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 */
@ExtendWith(JavaFxThreadInitializeExtension.class)
public class SizeUnitRadioTest {

    @Test
    public void requiredSizeUnit() {
        assertThrows(IllegalArgumentException.class, () -> new SizeUnitRadio(null));
    }

    @Test
    public void onSaveState() {
        SizeUnitRadio victim = new SizeUnitRadio(SizeUnit.KILOBYTE);
        victim.setSelected(true);
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertTrue(Boolean.parseBoolean(data.get(SizeUnit.KILOBYTE.toString())));
    }

    @Test
    public void onSaveStateNotSelected() {
        SizeUnitRadio victim = new SizeUnitRadio(SizeUnit.MEGABYTE);
        victim.setSelected(false);
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertFalse(Boolean.parseBoolean(data.get(SizeUnit.MEGABYTE.toString())));
    }

    @Test
    public void onRestoreState() {
        SizeUnitRadio victim = new SizeUnitRadio(SizeUnit.MEGABYTE);
        victim.setSelected(false);
        WorkspaceData.ToolData data = new WorkspaceData.ToolData();
        data.setBoolean(SizeUnit.MEGABYTE.toString(), true);
        victim.restoreStateFrom(data);
        assertTrue(victim.isSelected());
    }
}
