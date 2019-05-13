/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12 ott 2016
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
package org.pdfsam.ui.dashboard.preference;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.Test;
import org.pdfsam.context.BooleanUserPreference;
import org.pdfsam.context.UserContext;
import org.testfx.framework.junit.ApplicationTest;

import javafx.scene.Scene;
import javafx.scene.control.RadioButton;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

/**
 * @author Andrea Vacondio
 *
 */
public class PreferenceOutputPaneTest extends ApplicationTest {
    private UserContext userContext = mock(UserContext.class);

    @Override
    public void start(Stage stage) {
        PreferenceRadioButton smartRadio = new PreferenceRadioButton(BooleanUserPreference.SMART_OUTPUT, "radio", false,
                userContext);
        smartRadio.setId("smartRadio");
        PreferenceCheckBox compressionEnabled = new PreferenceCheckBox(BooleanUserPreference.PDF_COMPRESSION_ENABLED,
                "compression", true, userContext);
        compressionEnabled.setId("compressionEnabled");
        PreferenceOutputPane victim = new PreferenceOutputPane(smartRadio, compressionEnabled);
        victim.setId("victim");
        Scene scene = new Scene(new HBox(victim));
        stage.setScene(scene);
        stage.show();
    }

    @Test
    public void manualRadioIsSelected() {
        assertTrue(lookup("#manualRadio").queryAs(RadioButton.class).isSelected());
    }

    @Test
    public void clickCompression() {
        clickOn("#compressionEnabled");
        verify(userContext).setBooleanPreference(BooleanUserPreference.PDF_COMPRESSION_ENABLED, false);
    }

    @Test
    public void clickSmart() {
        clickOn("#smartRadio");
        verify(userContext).setBooleanPreference(BooleanUserPreference.SMART_OUTPUT, true);
    }

}
