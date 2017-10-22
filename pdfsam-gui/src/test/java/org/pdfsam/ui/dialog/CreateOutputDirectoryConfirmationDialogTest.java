/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 04 dic 2015
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
package org.pdfsam.ui.dialog;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Locale;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.SetLocaleEvent;
import org.pdfsam.test.ClearEventStudioRule;

import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.input.KeyCode;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class CreateOutputDirectoryConfirmationDialogTest extends GuiTest {
    private boolean confirm = false;

    @ClassRule
    public static ClearEventStudioRule CLEAR_STUDIO = new ClearEventStudioRule();

    @BeforeClass
    public static void setUp() {
        eventStudio().broadcast(new SetLocaleEvent(Locale.UK.toLanguageTag()));
    }

    @Override
    protected Parent getRootNode() {
        StylesConfig styles = mock(StylesConfig.class);
        CreateOutputDirectoryConfirmationDialog victim = new CreateOutputDirectoryConfirmationDialog(styles);
        Button button = new Button("show");
        button.setOnAction(a -> confirm = victim.response());
        return button;
    }

    @Test
    public void contentIsShown() {
        click("show");
        find(DefaultI18nContext.getInstance().i18n("The selected output directory does not exist"));
        find(DefaultI18nContext.getInstance().i18n("Do you want to create it?"));
        click(DefaultI18nContext.getInstance().i18n("No"));
    }

    @Test
    public void cancel() {
        this.confirm = true;
        click("show");
        click(DefaultI18nContext.getInstance().i18n("No"));
        assertFalse(this.confirm);
    }

    @Test
    @Ignore
    // TODO focus issue on Linux
    public void cancelOnEsc() {
        this.confirm = true;
        click("show");
        type(KeyCode.ESCAPE);
        assertFalse(this.confirm);
    }

    @Test
    public void overwrite() {
        this.confirm = false;
        click("show");
        click(DefaultI18nContext.getInstance().i18n("Yes"));
        assertTrue(this.confirm);
    }

    @Test
    @Ignore
    // TODO focus issue on Linux
    public void overwriteOnEnter() {
        this.confirm = false;
        click("show");
        type(KeyCode.ENTER);
        assertTrue(this.confirm);
    }

}
