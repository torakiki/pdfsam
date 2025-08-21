/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09/dic/2014
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
import javafx.stage.Stage;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.pdfsam.core.context.StringPersistentProperty;
import org.pdfsam.core.support.params.MultipleOutputTaskParametersBuilder;
import org.pdfsam.model.tool.TaskExecutionRequest;
import org.pdfsam.model.ui.workspace.WorkspaceData;
import org.pdfsam.persistence.PreferencesRepository;
import org.pdfsam.test.ClearEventStudioExtension;
import org.sejda.model.parameter.base.AbstractParameters;
import org.testfx.api.FxRobot;
import org.testfx.framework.junit5.ApplicationExtension;
import org.testfx.framework.junit5.Start;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ApplicationExtension.class, ClearEventStudioExtension.class })
@Tag("NoHeadless")
@SuppressWarnings({ "rawtypes", "unchecked" })
public class PrefixPaneTest {

    private MultipleOutputTaskParametersBuilder builder;
    private Consumer<String> onError;
    private PrefixPane victim;
    private PreferencesRepository repository;

    @BeforeEach
    public void setUp() {
        builder = mock(MultipleOutputTaskParametersBuilder.class);
        onError = mock(Consumer.class);
    }

    @Start
    public void start(Stage stage) {
        repository = new PreferencesRepository("/org/pdfsam/test");
        victim = new PrefixPane("module", repository);
        victim.setId("victim");
        Scene scene = new Scene(victim);
        stage.setScene(scene);
        stage.show();
    }

    @AfterEach
    public void tearDown() {
        repository.clean();
        app().persistentSettings().delete(StringPersistentProperty.PREFIX);
    }

    @Test
    public void apply() {
        victim.apply(builder, onError);
        verify(onError, never()).accept(anyString());
        verify(builder).prefix("PDFsam_");
    }

    @Test
    public void saveState() {
        Map<String, String> data = new HashMap<>();
        victim.saveStateTo(data);
        assertEquals("PDFsam_", data.get("victimprefix"));
    }

    @Test
    public void saveFieldValue(FxRobot robot) {
        robot.clickOn(p -> p instanceof PrefixField).write("ChuckNorris");
        eventStudio().broadcast(new TaskExecutionRequest("module", mock(AbstractParameters.class)));
        assertEquals("PDFsam_ChuckNorris", repository.getString("DEFAULT_PREFIX", ""));
    }

    @Test
    public void restoredFieldValue() {
        repository.saveString("DEFAULT_PREFIX", "Roundkick");
        PrefixPane pref = new PrefixPane("module", repository);
        assertEquals("Roundkick", pref.getText());
    }

    @Test
    public void restoreState() {
        WorkspaceData.ToolData data = new WorkspaceData.ToolData();
        data.set("victimprefix", "Chuck");
        victim.restoreStateFrom(data);
        assertEquals("Chuck", victim.getText());
    }

    @Test
    public void reset(FxRobot robot) {
        robot.clickOn(p -> p instanceof PrefixField).write("newPref");
        victim.resetView();
        assertEquals("PDFsam_", victim.getText());
    }

    @Test
    public void resetWithPersistentDefaultValue(FxRobot robot) {
        app().persistentSettings().set(StringPersistentProperty.PREFIX, "ChuckNorris");
        robot.clickOn(p -> p instanceof PrefixField).write("newPref");
        victim.resetView();
        assertEquals("ChuckNorris", victim.getText());
    }

    @Test
    public void resetWithPersistentValue(FxRobot robot) {
        app().persistentSettings().set(StringPersistentProperty.PREFIX, "ChuckNorris");
        repository.saveString("DEFAULT_PREFIX", "StevenSegal");
        victim.resetView();
        assertEquals("StevenSegal", victim.getText());
    }

}
