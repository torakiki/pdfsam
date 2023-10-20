/*
 * This file is part of the PDF Split And Merge source code
 * Created on 29/lug/2014
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
package org.pdfsam.ui.components.prefix;

import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.test.ClearEventStudioExtension;
import org.sejda.model.prefix.Prefix;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;
import org.testfx.util.WaitForAsyncUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
public class PrefixFieldTest {

    private PrefixField victim;

    @Start
    public void start(Stage stage) {
        victim = new PrefixField();
        Scene scene = new Scene(new HBox(victim));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    @Tag("NoHeadless")
    public void contextMenuAddsText(FxRobot robot) {
        robot.rightClickOn(victim).clickOn("#addPrefixMenu").clickOn(Prefix.BASENAME.getFriendlyName());
        WaitForAsyncUtils.waitForFxEvents();
        assertTrue(victim.getText().contains(Prefix.BASENAME.getFriendlyName()));
    }

    @Test
    @Tag("NoHeadless")
    public void contextMenuReplacesText(FxRobot robot) {
        robot.clickOn(victim).type(KeyCode.HOME).push(KeyCode.SHIFT, KeyCode.END);
        robot.rightClickOn(victim);
        robot.moveBy(5, 5);
        robot.clickOn();
        robot.clickOn(Prefix.BASENAME.getFriendlyName());
        WaitForAsyncUtils.waitForFxEvents();
        assertEquals(Prefix.BASENAME.getFriendlyName(), victim.getText());
    }

    @Test
    @Tag("NoHeadless")
    public void prefixMenuItemIsAdded(FxRobot robot) {
        victim.addMenuItemFor(Prefix.BOOKMARK);
        robot.rightClickOn(victim).clickOn("#addPrefixMenu").clickOn(Prefix.BOOKMARK.getFriendlyName());
        assertTrue(victim.getText().contains(Prefix.BOOKMARK.getFriendlyName()));
    }

    @Test
    @Tag("NoHeadless")
    public void prefixMenuItemIsAddedString(FxRobot robot) {
        victim.addMenuItemFor("Chuck");
        robot.rightClickOn(victim).clickOn("#addPrefixMenu").clickOn("Chuck");
        assertTrue(victim.getText().contains("Chuck"));
    }
}
