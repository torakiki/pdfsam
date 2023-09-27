/*
 * This file is part of the PDF Split And Merge source code
 * Created on 12 ott 2016
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
package org.pdfsam.gui.components.content.preference;

import javafx.scene.Scene;
import javafx.scene.control.RadioButton;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.ApplicationPersistentSettings;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class })
public class PreferenceOutputPaneTest {

    private final ApplicationContext appContext = mock(ApplicationContext.class);
    private final ApplicationPersistentSettings persistentSettings = mock(ApplicationPersistentSettings.class);
    private FxRobot robot;

    @Start
    public void start(Stage stage) {
        when(appContext.persistentSettings()).thenReturn(persistentSettings);
        PreferenceRadioButton smartRadio = new PreferenceRadioButton(BooleanPersistentProperty.SMART_OUTPUT, "radio",
                false, appContext);
        smartRadio.setId("smartRadio");
        PreferenceCheckBox compressionEnabled = new PreferenceCheckBox(
                BooleanPersistentProperty.PDF_COMPRESSION_ENABLED, "compression", true, appContext);
        compressionEnabled.setId("compressionEnabled");
        PreferenceCheckBox overwriteOutput = new PreferenceCheckBox(BooleanPersistentProperty.OVERWRITE_OUTPUT,
                "overwrite", false, appContext);
        overwriteOutput.setId("overwriteOutput");
        PreferenceOutputPane victim = new PreferenceOutputPane(smartRadio, compressionEnabled, overwriteOutput);
        victim.setId("victim");
        Scene scene = new Scene(new HBox(victim));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void manualRadioIsSelected() {
        assertTrue(robot.lookup("#manualRadio").queryAs(RadioButton.class).isSelected());
    }

    @Test
    public void clickCompression() {
        robot.clickOn("#compressionEnabled");
        verify(persistentSettings).set(BooleanPersistentProperty.PDF_COMPRESSION_ENABLED, false);
    }

    @Test
    public void clickSmart() {
        robot.clickOn("#smartRadio");
        verify(persistentSettings).set(BooleanPersistentProperty.SMART_OUTPUT, true);
    }

    @Test
    public void clickOverwrite() {
        robot.clickOn("#overwriteOutput");
        verify(persistentSettings).set(BooleanPersistentProperty.OVERWRITE_OUTPUT, true);
    }

}
