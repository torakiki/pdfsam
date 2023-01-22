package org.pdfsam.gui.components.sidebar;

import javafx.scene.control.ContentDisplay;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.test.JavaFxThreadInitializeExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 20/01/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@pdfsam.org).
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
@ExtendWith({ JavaFxThreadInitializeExtension.class })
class SidebarButtonTest {
    @Test
    public void contentDisplay() {
        var victim = new SidebarButton();
        assertFalse(victim.isDisplayText());
        assertEquals(ContentDisplay.GRAPHIC_ONLY, victim.getContentDisplay());
        victim.setDisplayText(true);
        assertTrue(victim.isDisplayText());
        assertEquals(ContentDisplay.LEFT, victim.getContentDisplay());
    }
}