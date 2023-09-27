/*
 * This file is part of the PDF Split And Merge source code
 * Created on 26 ott 2015
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
package org.pdfsam.service.news;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.core.AppBrand;
import org.pdfsam.core.BrandableProperty;
import org.pdfsam.model.news.NewsData;
import org.pdfsam.persistence.PersistenceException;
import org.pdfsam.persistence.PreferencesRepository;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.pdfsam.service.news.DefaultNewsService.LATEST_IMPORTANT_NEWS_ID;
import static org.pdfsam.service.news.DefaultNewsService.LATEST_NEWS_ID;

/**
 * @author Andrea Vacondio
 */
public class DefaultNewsServiceTest {

    private DefaultNewsService victim;
    private AppBrand appBrand;
    private PreferencesRepository repo;

    @BeforeEach
    public void setUp() {
        appBrand = mock(AppBrand.class);
        var mapper = JsonMapper.builder().addModule(new Jdk8Module()).addModule(new JavaTimeModule())
                .enable(DeserializationFeature.FAIL_ON_NUMBERS_FOR_ENUMS).enable(SerializationFeature.INDENT_OUTPUT)
                .disable(SerializationFeature.WRITE_DATE_TIMESTAMPS_AS_NANOSECONDS)
                .visibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY)
                .serializationInclusion(JsonInclude.Include.NON_EMPTY)
                .build();
        repo = mock(PreferencesRepository.class);
        victim = new DefaultNewsService(appBrand, mapper, repo);
    }

    @Test
    public void testSetLatestNewsSeen() {
        victim.setLatestNewsSeen(5);
        verify(repo).saveInt(LATEST_NEWS_ID, 5);
    }

    @Test
    @DisplayName("Exception doesn't propagate")
    public void testSetLatestNewsSeenNegative() {
        doThrow(new PersistenceException("msg")).when(repo).saveInt(any(), anyInt());
        victim.setLatestNewsSeen(5);
    }

    @Test
    public void testSetLatestImportantNewsSeen() {
        victim.setLatestImportantNewsSeen(5);
        verify(repo).saveInt(LATEST_IMPORTANT_NEWS_ID, 5);
    }

    @Test
    @DisplayName("Exception doesn't propagate")
    public void testSetLatestImportantNewsSeenNegative() {
        doThrow(new PersistenceException("msg")).when(repo).saveInt(any(), anyInt());
        victim.setLatestImportantNewsSeen(5);
    }

    @Test
    public void testClear() {
        victim.clear();
        verify(repo).clean();
    }

    @Test
    @DisplayName("Exception doesn't propagate")
    public void testClearNegative() {
        doThrow(new PersistenceException("msg")).when(repo).clean();
        victim.setLatestImportantNewsSeen(5);
    }

    @Test
    public void testGetLatestNews(@TempDir Path folder) throws Exception {
        var file = Files.createTempFile(folder, null, null);
        Files.copy(getClass().getResourceAsStream("/test_news.json"), file, StandardCopyOption.REPLACE_EXISTING);
        when(appBrand.property(BrandableProperty.NEWS_URL)).thenReturn(file.toFile().toURI().toString());
        List<NewsData> news = victim.getLatestNews();
        assertEquals(1, news.size());
        assertEquals("news-title", news.get(0).title());
        assertEquals("news-content", news.get(0).content());
        assertEquals(21, news.get(0).id());
        assertEquals("http://www.pdfsam.org/", news.get(0).link());
        assertNotNull(news.get(0).date());
        assertTrue(news.get(0).important());
    }

}
