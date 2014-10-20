/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/ott/2014
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
package org.pdfsam.ui.dialog;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Locale;

import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.input.KeyCode;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.i18n.SetLocaleEvent;
import org.pdfsam.test.ClearEventStudioRule;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class OverwriteConfirmationDialogTest extends GuiTest {

    private boolean overwrite = false;

    @ClassRule
    public static ClearEventStudioRule CLEAR_STUDIO = new ClearEventStudioRule();

    @BeforeClass
    public static void setUp() {
        eventStudio().broadcast(new SetLocaleEvent(Locale.UK.toLanguageTag()));
    }

    @Override
    protected Parent getRootNode() {
        StylesConfig styles = mock(StylesConfig.class);
        OverwriteConfirmationDialog victim = new OverwriteConfirmationDialog(styles);
        Button button = new Button("show");
        button.setOnAction(a -> overwrite = victim.title("Title").messageTitle("MessageTitle")
                .messageContent("MessageContent").shouldOverwrite());
        return button;
    }

    @Test
    public void contentIsShown() {
        click("show");
        find("MessageTitle");
        find("MessageContent");
        click("Cancel");
    }

    @Test
    public void cancel() {
        this.overwrite = true;
        click("show");
        click("Cancel");
        assertFalse(this.overwrite);
    }

    @Test
    public void overwrite() {
        this.overwrite = false;
        click("show");
        click("Overwrite");
        assertTrue(this.overwrite);
    }

    @Test
    @Ignore
    public void esc() {
        this.overwrite = true;
        click("show");
        push(KeyCode.ESCAPE);
        assertFalse(this.overwrite);
    }

}
