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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Locale;

import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.TemporaryFolder;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.SetLocaleEvent;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.commons.NonExistingOutputDirectoryEvent;
import org.sejda.injector.Components;
import org.sejda.injector.Injector;
import org.sejda.injector.Provides;

import javafx.scene.Parent;
import javafx.scene.control.Button;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
public class CreateOutputDirectoryDialogControllerTest extends GuiTest {
    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule();

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @BeforeClass
    public static void setUp() {
        eventStudio().broadcast(new SetLocaleEvent(Locale.UK.toLanguageTag()));
    }

    @Override
    protected Parent getRootNode() {
        Injector.start(new Config());
        Button button = new Button("show");
        return button;
    }

    @Components({ CreateOutputDirectoryDialogController.class })
    static class Config {

        @Provides
        StylesConfig style() {
            return mock(StylesConfig.class);
        }

    }

    @Test
    public void negativeTest() throws IOException {
        Button button = find("show");
        Path file = Paths.get(folder.newFolder().getAbsolutePath());
        button.setOnAction(a -> eventStudio().broadcast(new NonExistingOutputDirectoryEvent(file)));
        folder.delete();
        click("show");
        click(DefaultI18nContext.getInstance().i18n("No"));
        assertFalse(Files.exists(file));
    }

    @Test
    public void positiveTest() throws IOException {
        Button button = find("show");
        Path file = Paths.get(folder.newFolder().getAbsolutePath());
        button.setOnAction(a -> eventStudio().broadcast(new NonExistingOutputDirectoryEvent(file)));
        folder.delete();
        click("show");
        click(DefaultI18nContext.getInstance().i18n("Yes"));
        assertTrue(Files.exists(file));
    }
}
