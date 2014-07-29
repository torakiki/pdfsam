/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/lug/2014
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
package org.pdfsam.ui.prefix;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import javafx.scene.Parent;
import javafx.scene.input.KeyCode;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.model.prefix.Prefix;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class PrefixFieldTest extends GuiTest {

    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule("LogStage");

    @Override
    protected Parent getRootNode() {
        PrefixField victim = new PrefixField();
        victim.getStyleClass().add("victim");
        return victim;
    }

    @Test
    public void contextMenuAddsText() {
        PrefixField victim = find(".victim");
        rightClick(victim);
        moveBy(5, 5);
        click();
        click(Prefix.BASENAME.getFriendlyName());
        assertTrue(victim.getText().contains(Prefix.BASENAME.getFriendlyName()));
    }

    @Test
    public void contextMenuReplacesText() {
        PrefixField victim = find(".victim");
        click(victim).type(KeyCode.HOME).push(KeyCode.SHIFT, KeyCode.END);
        rightClick(victim);
        moveBy(5, 5);
        click();
        click(Prefix.BASENAME.getFriendlyName());
        assertEquals(Prefix.BASENAME.getFriendlyName(), victim.getText());
    }
}
