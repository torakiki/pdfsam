/*
 * This file is part of the PDF Split And Merge source code
 * Created on 05/dic/2014
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
package org.pdfsam.model.ui.workspace;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;

import static java.util.Objects.isNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * @author Andrea Vacondio
 *
 */
@SuppressWarnings("unused")
public class SaveWorkspaceEventTest {

    private SaveWorkspaceRequest victim;

    @BeforeEach
    public void setUp() {
        victim = new SaveWorkspaceRequest(mock(File.class));
    }

    @Test
    public void requiredFile() {
        assertThrows(IllegalArgumentException.class, () -> new SaveWorkspaceRequest(null));
    }

    @Test
    public void nonNullKey() {
        assertThrows(IllegalArgumentException.class, () -> victim.addValue("module", null, "value"));
    }

    @Test
    public void nullValue() {
        victim.addValue("module", "key", null);
        assertEquals("", victim.getDataForTool("module").get("key"));
    }

    @Test
    public void add() {
        assertTrue(victim.getData().isEmpty());
        victim.addValue("module", "key", "value");
        assertFalse(victim.getData().isEmpty());
    }

    @Test
    public void addToTheRightModule() {
        victim.addValue("module", "key", "value");
        assertFalse(victim.getData().get("module").isEmpty());
        assertTrue(isNull(victim.getData().get("chuck")));
    }

    @Test
    public void multipleAdd() {
        victim.addValue("module", "key", "value");
        victim.addValue("module", "key2", "value2");
        assertEquals(2, victim.getDataForTool("module").size());
    }

    @Test
    public void nullSafeGet() {
        assertEquals(0, victim.getDataForTool("chuck").size());
    }
}
