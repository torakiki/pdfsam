/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/lug/2014
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
package org.pdfsam.ui.prefix;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.pdfsam.NoHeadless;
import org.pdfsam.test.ClearEventStudioRule;
import org.sejda.model.prefix.Prefix;
import org.testfx.framework.junit.ApplicationTest;
import org.testfx.util.WaitForAsyncUtils;

import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class PrefixFieldTest extends ApplicationTest {

    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule();
    private PrefixField victim;
    private PrefixField secondVictim;

    @Override
    public void start(Stage stage) {
        victim = new PrefixField(null);
        secondVictim = new PrefixField("ChuckNorris");
        Scene scene = new Scene(new HBox(victim, secondVictim));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void defaultValue() {
        assertEquals("PDFsam_", victim.getText());
    }

    @Test
    public void placeHolderValue() {
        assertEquals("ChuckNorris", secondVictim.getText());
    }

    @Test
    @Category(NoHeadless.class)
    public void contextMenuAddsText() {
        rightClickOn(victim).clickOn("#addPrefixMenu").clickOn(Prefix.BASENAME.getFriendlyName());
        WaitForAsyncUtils.waitForFxEvents();
        assertTrue(victim.getText().contains(Prefix.BASENAME.getFriendlyName()));
    }

    @Test
    @Category(NoHeadless.class)
    public void contextMenuReplacesText() {
        clickOn(victim).type(KeyCode.HOME).push(KeyCode.SHIFT, KeyCode.END);
        rightClickOn(victim);
        moveBy(5, 5);
        clickOn();
        clickOn(Prefix.BASENAME.getFriendlyName());
        WaitForAsyncUtils.waitForFxEvents();
        assertEquals(Prefix.BASENAME.getFriendlyName(), victim.getText());
    }

    @Test
    @Category(NoHeadless.class)
    public void prefixMenuItemIsAdded() {
        victim.addMenuItemFor(Prefix.BOOKMARK);
        rightClickOn(victim).clickOn("#addPrefixMenu").clickOn(Prefix.BOOKMARK.getFriendlyName());
        assertTrue(victim.getText().contains(Prefix.BOOKMARK.getFriendlyName()));
    }

    @Test
    @Category(NoHeadless.class)
    public void prefixMenuItemIsAddedString() {
        victim.addMenuItemFor("Chuck");
        rightClickOn(victim).clickOn("#addPrefixMenu").clickOn("Chuck");
        assertTrue(victim.getText().contains("Chuck"));
    }
}
