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
import org.junit.rules.TemporaryFolder;
import org.pdfsam.configuration.StylesConfig;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.SetLocaleEvent;
import org.pdfsam.injector.Components;
import org.pdfsam.injector.Injector;
import org.pdfsam.injector.Provides;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.commons.NonExistingOutputDirectoryEvent;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class CreateOutputDirectoryDialogControllerTest extends ApplicationTest {
    @Rule
    public ClearEventStudioRule clearEventStudio = new ClearEventStudioRule();

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private Button button;

    @BeforeClass
    public static void setUp() {
        eventStudio().broadcast(new SetLocaleEvent(Locale.UK.toLanguageTag()));
    }

    @Override
    public void start(Stage stage) {
        Injector.start(new Config());
        button = new Button("show");
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
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
        Path file = Paths.get(folder.newFolder().getAbsolutePath());
        button.setOnAction(a -> eventStudio().broadcast(new NonExistingOutputDirectoryEvent(file)));
        folder.delete();
        clickOn("show");
        clickOn(DefaultI18nContext.getInstance().i18n("No"));
        assertFalse(Files.exists(file));
    }

    @Test
    public void positiveTest() throws IOException {
        Path file = Paths.get(folder.newFolder().getAbsolutePath());
        button.setOnAction(a -> eventStudio().broadcast(new NonExistingOutputDirectoryEvent(file)));
        folder.delete();
        clickOn("show");
        clickOn(DefaultI18nContext.getInstance().i18n("Yes"));
        assertTrue(Files.exists(file));
    }
}
