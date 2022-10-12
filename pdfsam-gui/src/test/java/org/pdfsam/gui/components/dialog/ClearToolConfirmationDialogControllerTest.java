/*
 * This file is part of the PDF Split And Merge source code
 * Created on 25 ott 2020
 * Copyright 2019 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.gui.components.dialog;

import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.RegisterExtension;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.ApplicationPersistentSettings;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.pdfsam.i18n.SetLocaleRequest;
import org.pdfsam.model.tool.ClearToolRequest;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.HitTestListener;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class })
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ClearToolConfirmationDialogControllerTest {

    private static final String TOOL = "Tool";
    @RegisterExtension
    public static ClearEventStudioExtension extension = new ClearEventStudioExtension(TOOL);
    private Button button;
    private HitTestListener<ClearToolRequest> listener;
    private final ApplicationContext appContext = mock(ApplicationContext.class);
    private final ApplicationPersistentSettings persistentSettings = mock(ApplicationPersistentSettings.class);
    private FxRobot robot;

    @BeforeAll
    public static void setUp() {
        i18n().setLocale(new SetLocaleRequest(Locale.UK.toLanguageTag()));
    }

    @Start
    public void start(Stage stage) {
        app().persistentSettings().set(BooleanPersistentProperty.CLEAR_CONFIRMATION, Boolean.TRUE);
        button = new Button("show");
        new ClearToolConfirmationDialogController(() -> new ClearToolConfirmationDialog(stage));
        Scene scene = new Scene(new VBox(button));
        stage.setScene(scene);
        stage.show();
        listener = new HitTestListener<>();
    }

    @Test
    public void negativeTest() {
        button.setOnAction(a -> eventStudio().broadcast(new ClearToolRequest(TOOL, true, true)));
        eventStudio().add(ClearToolRequest.class, listener, TOOL);
        robot.clickOn("show");
        robot.clickOn(i18n().tr("No"));
        assertFalse(listener.isHit());
    }

    @Test
    public void noAskConfirmation() {
        button.setOnAction(a -> eventStudio().broadcast(new ClearToolRequest(TOOL, true, false)));
        eventStudio().add(ClearToolRequest.class, listener, TOOL);
        robot.clickOn("show");
        assertTrue(listener.isHit());
    }

    @Test
    @Order(Integer.MAX_VALUE)
    //make sure it's the last since we change the mock return
    public void noAskConfirmationPreference() {
        app().persistentSettings().set(BooleanPersistentProperty.CLEAR_CONFIRMATION, Boolean.FALSE);
        button.setOnAction(a -> eventStudio().broadcast(new ClearToolRequest(TOOL, true, true)));
        eventStudio().add(ClearToolRequest.class, listener, TOOL);
        robot.clickOn("show");
        assertTrue(listener.isHit());
    }

    @Test
    public void positiveTest() {
        button.setOnAction(a -> eventStudio().broadcast(new ClearToolRequest(TOOL, true, true)));
        eventStudio().add(ClearToolRequest.class, listener, TOOL);
        robot.clickOn("show");
        robot.clickOn(i18n().tr("Yes"));
        assertTrue(listener.isHit());
    }
}
